{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module      : Butter.Core.Tracker.Client
-- Copyright   : Pedro Tacla Yamada
-- License     : MIT (see LICENSE)
--
-- Maintainer  : Pedro Tacla Yamada <tacla.yamada@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- A Wrapper library for the Zulip API. Works on top of a 'ReaderT' monad
-- transformer, holding a 'ZulipOptions' object, which should hold the
-- state and configuration for the API client.
--
-- Using the library is made easier through a set of helper functions. This
-- design is more concise and than passing around configuration variables;
-- one could easily bypass it with the use of 'runZulip', though that isn't
-- recommended.
--
-- Example usage:
-- > import Web.HZulip
-- >
-- > main :: IO ()
-- > main = withZulipCreds "zulip-api-user" "zulip-api-key" $ do
-- >     -- Since we are inside the ZulipM ReaderT monad transformer, we
-- >     -- don't need to pass options around. The above function already
-- >     -- created an HTTP manager, for connection pooling and wrapped the
-- >     -- default configuration options with the Monad:
-- >     print =<< getSubscriptions
-- >     -- >> ["haskell"]
-- >
-- >     -- Sending messages is as easy as:
-- >     void $ sendStreamMessage "haskell"              -- message stream
-- >                              "hzulip"               -- message topic
-- >                              "Message from Haskell" -- message content
-- >
-- >     -- Listening for events works with a callback based API. More
-- >     -- complex patterns for concurrent message handling can be created
-- >     -- from it. As long as your zulip user is already subscribed to
-- >     -- streams, this is all you have to do:
-- >     onNewEvent ["message"] $ \msg -> do
-- >         liftIO $ putStrLn "Got a new message!"
-- >         let usr = messageSender msg
-- >             fn = userFullName usr
-- >             e = userEmail usr
-- >
-- >        sendPrivateMessage [e] $ "Thanks for the message " ++ fn ++ "!!"
--
module Web.HZulip ( Event(..)
                  , Message(..)
                  , Queue(..)
                  , User(..)
                  , ZulipOptions(..)
                  , EventCallback
                  , MessageCallback
                  , addSubscriptions
                  , defaultBaseUrl
                  , eventTypes
                  , getEvents
                  , getStreams
                  , getStreamSubscribers
                  , getSubscriptions
                  , onNewEvent
                  , onNewMessage
                  , registerQueue
                  , removeSubscriptions
                  , runZulip
                  , sendMessage
                  , sendPrivateMessage
                  , sendStreamMessage
                  , withZulip
                  , withZulipCreds
                  , zulipOptions
                  )
  where

import Control.Arrow (second)
import Control.Concurrent (threadDelay)
import Control.Lens ((^..))
import Control.Monad (void)
import Control.Monad.Catch (SomeException, handleAll)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Data.Aeson (decode)
import Data.Aeson.Lens (key, values, _String)
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.ByteString.Char8 as C (pack)
import qualified Data.ByteString.Lazy.Char8 as CL (unpack)
import Data.Text as T (Text, unpack)
import Data.Text.Encoding as T (encodeUtf8)
import Network.HTTP.Client (Request, applyBasicAuth, httpLbs, method,
                            newManager, parseUrl, responseBody, setQueryString)
import Network.HTTP.Client.MultipartFormData (formDataBody, partBS)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (Method, methodGet, methodPatch, methodPost)

import Web.HZulip.Types as ZT

-- Public functions:
-------------------------------------------------------------------------------

-- |
-- Helper for creating a `ZulipOptions` object with the `baseUrl` set to
-- `defaultBaseUrl`
zulipOptions :: String -> String -> IO ZulipOptions
zulipOptions e k = do
    manager <- newManager tlsManagerSettings
    return $ ZulipOptions e k defaultBaseUrl manager

-- |
-- Helper to run Actions in the Zulip Monad
runZulip :: ZulipM a -> ZulipOptions -> IO a
runZulip = runReaderT

-- |
-- Flipped version of 'runZulip'
withZulip :: ZulipOptions -> ZulipM a -> IO a
withZulip = flip runZulip

-- |
-- Helper for creating a minimal 'ZulipOptions' object and running an action
-- in the 'ZulipM' monad
withZulipCreds :: String -> String -> ZulipM a -> IO a
withZulipCreds e k action = do
    opts <- zulipOptions e k
    runZulip action opts

-- |
-- The default zulip API URL
defaultBaseUrl :: String
defaultBaseUrl = "https://api.zulip.com/v1"

-- |
-- The list of all avaiable event types
eventTypes :: [String]
eventTypes = ["message", "subscriptions", "realm_user", "pointer"]

-- |
-- This wraps `POST https://api.zulip.com/v1/messages` with a nicer root
-- API. Simpler helpers for each specific case of this somewhat overloaded
-- endpoint will also be provided in the future.
--
-- It takes the message `mtype`, `mrecipients`, `msubject` and `mcontent`
-- and returns the created message's `id` in the `ZulipM` monad.
sendMessage :: String -> [String] -> String -> String -> ZulipM Int
sendMessage mtype mrecipients msubject mcontent = do
    let form = [ ("type"   , mtype)
               , ("content", mcontent)
               , ("to"     , show mrecipients)
               , ("subject", msubject)
               ]

    body <- zulipMakeRequest Messages methodPost form >>= decodeResponse
    let Just mid = responseMessageId body in return mid

-- |
-- Helper for sending private messages. Takes the list of recipients and
-- the message's content.
sendPrivateMessage :: [String] -> String -> ZulipM Int
sendPrivateMessage mrs = sendMessage "private" mrs ""

-- |
-- Helper for sending stream messages. Takes the stream name, the subject
-- and the message.
sendStreamMessage :: String -> String -> String -> ZulipM Int
sendStreamMessage s = sendMessage "stream" [s]

-- |
-- This registers a new event queue with the zulip API. It's a lower level
-- function, which shouldn't be used unless you know what you're doing. It
-- takes a list of names of the events you want to listen for and whether
-- you'd like for the content to be rendered in HTML format
-- (if you set the last parameter to `False` it will be kept as typed, in
-- markdown format)
registerQueue :: [String] -> Bool -> ZulipM Queue
registerQueue evTps mdn = do
    let form = [ ("event_types"   , show evTps)
               , ("apply_markdown", if mdn then "true" else "false")
               ]

    body <- zulipMakeRequest Register methodPost form >>= decodeResponse

    let Just qid = responseQueueId body
        Just lid = responseLastEventId body
      in return $ Queue qid lid

-- |
-- Get a list of all the public streams
getStreams :: ZulipM [String]
getStreams = do
    r <- zulipMakeRequest Streams methodGet []
    return $ map T.unpack $ r ^.. key "streams" . values
                                . key "name" . _String

-- |
-- Get all the user emails subscribed to a stream
getStreamSubscribers :: String -> ZulipM [String]
getStreamSubscribers s = do
    r <- zulipMakeRequest' ("/streams/" ++ s ++ "/members") methodGet []
    return $ map T.unpack $ r ^.. key "subscribers" . values . _String

-- |
-- Get a list of the streams the client is currently subscribed to.
getSubscriptions :: ZulipM [String]
getSubscriptions = do
    r <- zulipMakeRequest Subscriptions methodGet []
    return $ map T.unpack $ r ^.. key "subscriptions" . values
                                . key "name" . _String

-- |
-- Add new Stream subscriptions to the client.
addSubscriptions :: [String] -> ZulipM ()
addSubscriptions sbs = do
    let form = [ ("subscriptions", show sbs) ]
    void $ zulipMakeRequest Subscriptions methodPost form

-- |
-- Remove one or more Stream subscriptions from the client
removeSubscriptions :: [String] -> ZulipM ()
removeSubscriptions sbs = do
    let form = [ ("delete", show sbs ) ]
    void $ zulipMakeRequest Subscriptions methodPatch form

-- |
-- Fetches new set of events from a `Queue`.
getEvents :: Queue -> Bool -> ZulipM (Queue, [Event])
getEvents q b = do
    let qs = [ ("queue_id"     , queueId q)
             , ("last_event_id", show $ lastEventId q)
             , ("dont_block"   , if b then "true" else "false")
             ]

    body <- zulipMakeRequest Events methodGet qs >>= decodeResponse
    let Just evs = responseEvents body
        -- Get the last event id and pass it back with the `Queue`
        lEvId = maximum $ map eventId evs
      in return (q { lastEventId = lEvId }, evs)

-- |
-- Registers an event callback for specified events and keeps executing it
-- over events as they come in. Will loop forever
onNewEvent :: [String] -> EventCallback -> ZulipM ()
onNewEvent etypes f = do
    q <- registerQueue etypes False
    handleAll (tryAgain q) (loop q)
  where tryAgain :: Queue -> SomeException -> ZulipM ()
        tryAgain q _ = do
            liftIO (threadDelay 1000000)
            handleAll (tryAgain q) (loop q)
        loop q = getEvents q False >>=
                 \(q', evts) -> mapM_ f evts >>
                                loop q'

-- |
-- Registers a callback to be executed whenever a message comes in. Will
-- loop forever
onNewMessage :: MessageCallback -> ZulipM ()
onNewMessage f = onNewEvent ["message"] $ \evt ->
  -- I could just pattern match here, as I did in other places and simply
  -- expect the Zulip API not to give us correct responses, but I think
  -- this is more reasonable.
  maybe (return ()) f (eventMessage evt)

-- Private functions:
-------------------------------------------------------------------------------

data Endpoint = Messages | Register | Events | Subscriptions | Streams
type RequestData = [(T.Text, String)]

-- |
-- Makes a request to some 'Endpoint' in the zulip API
zulipMakeRequest :: Endpoint -> Method -> RequestData -> ZulipM BL.ByteString
zulipMakeRequest e = zulipMakeRequest' (endpointSuffix e)

-- |
-- Makes a request to some untyped URL in the zulip API. Serializes the
-- 'RequestData' as a QueryString on GET requests and as form-data
-- otherwise
zulipMakeRequest' :: String -> Method -> RequestData -> ZulipM BL.ByteString
zulipMakeRequest' u m d = do
    z <- ask
    req  <- liftIO $ parseUrl (clientBaseUrl z ++ u)
    req' <- prepareRequest d req
    res  <- liftIO $ httpLbs req' { method = m } $ clientManager z
    return $ responseBody res

-- |
-- A helper for decoding a response in the Zulip monad
decodeResponse :: BL.ByteString -> ZulipM ZT.Response
decodeResponse b = case decode b of
    Just r -> if wasSuccessful r then return r
                                 else fail $ responseMsg r
    _ -> fail $ "Unexpected response from the Zulip API: " ++ CL.unpack b

-- |
-- Adds a QueryString or FormData body, represented by a 'RequestData' list
-- of tuples, and authenticates the request, with the current zulip state's
-- credentials.
prepareRequest :: RequestData -> Request -> ZulipM Request
prepareRequest [] r = applyAuth r
prepareRequest d r | method r == methodGet =
    applyAuth $ setQueryString (map helper d) r
  where helper (k, v) = (encodeUtf8 k, Just $ C.pack v)
prepareRequest d r =
    applyAuth =<< formDataBody (map (uncurry partBS . second C.pack) d) r

-- |
-- Adds authentication to a 'Request' with the configuration in the 'ZulipM'
-- monad
applyAuth :: Request -> ZulipM Request
applyAuth req = do
      ZulipOptions e k _ _ <- ask
      return $ applyBasicAuth (C.pack e) (C.pack k) req

-- |
-- Returns `True` if a response indicates success
wasSuccessful :: ZT.Response -> Bool
wasSuccessful = (== ResponseSuccess) . responseResult

-- |
-- Gets the suffix for some endpoint
endpointSuffix :: Endpoint -> String
endpointSuffix Messages      = "/messages"
endpointSuffix Events        = "/events"
endpointSuffix Register      = "/register"
endpointSuffix Subscriptions = "/users/me/subscriptions"
endpointSuffix Streams       = "/streams"
