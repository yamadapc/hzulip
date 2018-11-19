{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module      : Web.HZulip
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
-- Examples are available at the github repository for this project at:
-- https://github.com/yamadapc/hzulip
module Web.HZulip ( Event(..)
                  , Message(..)
                  , Queue(..)
                  , User(..)
                  , Profile(..)
                  , ZulipOptions(..)
                  , ZulipM
                  , EventCallback
                  , MessageCallback
                  , addSubscriptions
                  , addAllSubscriptions
                  , defaultBaseUrl
                  , eventTypes
                  , getEvents
                  , getProfile
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
                  , sinkZulipMessages
                  , sourceZulipEvents
                  , sourceZulipMessages
                  , withZulip
                  , withZulipCreds
                  , zulipOptions

                  , lift
                  , ask
                  )
  where

import Control.Arrow (second)
import Control.Concurrent.STM (TBQueue, atomically, writeTBQueue)
import Control.Lens ((^..))
import Control.Monad (void)
import Control.Monad.Catch (catch, throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Data.Aeson (ToJSON, eitherDecode', (.=), object)
import Data.Aeson.Lens (key, values, _String)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.ByteString.Lazy.Char8 as CL (unpack)
import Data.Conduit (Sink, Source, await)
import Data.Conduit.Async (gatherFrom)
import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.Text as T (Text, unpack)
import qualified Data.Text as T
import Data.Text.Encoding as T (encodeUtf8)
import qualified Data.Text.Lazy as TL
import Network.HTTP.Client (Request, HttpException(..), HttpExceptionContent(ResponseTimeout), applyBasicAuth, httpLbs,
                            method, newManager, parseUrl, responseBody,
                            setQueryString)
import Network.HTTP.Client.MultipartFormData (formDataBody, partBS)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (Method, methodGet, methodPatch, methodPost)

import Web.HZulip.Types as ZT

-- Public functions:
-------------------------------------------------------------------------------

-- |
-- Helper for creating a `ZulipOptions` object with the `baseUrl` set to
-- `defaultBaseUrl`
zulipOptions :: Text -> Text -> IO ZulipOptions
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
withZulipCreds :: Text -> Text -> ZulipM a -> IO a
withZulipCreds e k action = do
    opts <- zulipOptions e k
    runZulip action opts

-- |
-- Get the current user's profile.
getProfile :: ZulipM Profile
getProfile = do
  r <- zulipMakeRequest EndpointProfile methodGet []
  case eitherDecode' r of
    Right u -> return u
    Left err -> fail $ "getProfile: Unexpected response from the Zulip API: " ++ CL.unpack r ++ "\n"
                       ++ "Error was: " ++ err


-- |
-- The default zulip API URL
defaultBaseUrl :: Text
defaultBaseUrl = "https://api.zulip.com/v1"

-- |
-- The list of all avaiable event types
eventTypes :: [Text]
eventTypes = ["message", "subscriptions", "realm_user", "pointer"]

-- |
-- Encode a JSON Value to a `Text`.
encodeToText :: (ToJSON a) => a -> Text
encodeToText = TL.toStrict . encodeToLazyText

-- |
-- This wraps `POST https://api.zulip.com/v1/messages` with a nicer root
-- API. Simpler helpers for each specific case of this somewhat overloaded
-- endpoint will also be provided in the future.
--
-- It takes the message `mtype`, `mrecipients`, `msubject` and `mcontent`
-- and returns the created message's `id` in the `ZulipM` monad.
sendMessage :: Text -> [Text] -> Text -> Text -> ZulipM Int
sendMessage mtype mrecipients msubject mcontent = do
    let form = [ ("type"   , mtype)
               , ("content", mcontent)
               , ("to"     , encodeToText mrecipients)
               , ("subject", msubject)
               ]

    body <- zulipMakeRequest EndpointMessages methodPost form >>= decodeResponse
    let Just mid = responseMessageId body in return mid

-- |
-- Helper for sending private messages. Takes the list of recipients and
-- the message's content.
sendPrivateMessage :: [Text] -> Text -> ZulipM Int
sendPrivateMessage mrs = sendMessage "private" mrs ""

-- |
-- Helper for sending stream messages. Takes the stream name, the subject
-- and the message.
sendStreamMessage :: Text -> Text -> Text -> ZulipM Int
sendStreamMessage s = sendMessage "stream" [s]

-- |
-- This registers a new event queue with the zulip API. It's a lower level
-- function, which shouldn't be used unless you know what you're doing. It
-- takes a list of names of the events you want to listen for and whether
-- you'd like for the content to be rendered in HTML format
-- (if you set the last parameter to `False` it will be kept as typed, in
-- markdown format)
registerQueue :: [Text] -> Bool -> ZulipM Queue
registerQueue evTps mdn = do
    let form = [ ("event_types"   , encodeToText evTps)
               , ("apply_markdown", if mdn then "true" else "false")
               ]

    body <- zulipMakeRequest EndpointRegister methodPost form >>= decodeResponse

    let Just qid = responseQueueId body
        Just lid = responseLastEventId body
      in return $ Queue qid lid

-- |
-- Get a list of all the public streams
getStreams :: ZulipM [Text]
getStreams = do
    r <- zulipMakeRequest EndpointStreams methodGet []
    return $ r ^.. key "streams" . values
                 . key "name" . _String

-- |
-- Get all the user emails subscribed to a stream
getStreamSubscribers :: Text -> ZulipM [Text]
getStreamSubscribers s = do
    r <- zulipMakeRequest' ("/streams/" <> s <> "/members") methodGet []
    return $ r ^.. key "subscribers" . values . _String

-- |
-- Get a list of the streams the client is currently subscribed to.
getSubscriptions :: ZulipM [Text]
getSubscriptions = do
    r <- zulipMakeRequest EndpointSubscriptions methodGet []
    return $ r ^.. key "subscriptions" . values
                 . key "name" . _String

-- |
-- Subscribes the client to all available streams and returns all the
-- stream names
addAllSubscriptions :: ZulipM [Text]
addAllSubscriptions = do
    ss <- getStreams
    addSubscriptions ss
    return ss

-- |
-- Add new Stream subscriptions to the client.
addSubscriptions :: [Text] -> ZulipM ()
addSubscriptions sbs = do
    let sbs' = encodeToText $ [ object ["name" .= sb] | sb <- sbs ]
        form = [ ("add", sbs') ]
    void $ zulipMakeRequest EndpointSubscriptions methodPatch form

-- |
-- Remove one or more Stream subscriptions from the client
removeSubscriptions :: [Text] -> ZulipM ()
removeSubscriptions sbs = do
    let form = [ ("delete", encodeToText sbs ) ]
    void $ zulipMakeRequest EndpointSubscriptions methodPatch form

-- |
-- Fetches new set of events from a `Queue`.
getEvents :: Queue -> Bool -> ZulipM (Queue, [Event])
getEvents q b = do
    let qs = [ ("queue_id"     , queueId q)
             , ("last_event_id", encodeToText $ lastEventId q)
             , ("dont_block"   , encodeToText b)
             ]

    body <- zulipMakeRequest EndpointEvents methodGet qs >>= decodeResponse
    let Just evs = responseEvents body
        -- Get the last event id and pass it back with the `Queue`
        lEvId = maximum $ map eventId evs
      in return (q { lastEventId = lEvId }, evs)

-- |
-- Registers an event callback for specified events and keeps executing it
-- over events as they come in
onNewEvent :: [Text] -> EventCallback -> ZulipM ()
onNewEvent etypes f = do
    q <- registerQueue etypes False
    -- We let it fail here, so that failures can be catched and handled by
    -- the user
    loop q
  where getEvents' q = catch (getEvents q False) (onTimeout q)
        loop q = do
            (q', evts) <- getEvents' q
            mapM_ f evts
            loop q'
        onTimeout q (HttpExceptionRequest _req ResponseTimeout) = getEvents' q
        onTimeout _ ex = throwM ex

-- |
-- Registers a callback to be executed whenever a message comes in. Will
-- loop forever
onNewMessage :: MessageCallback -> ZulipM ()
onNewMessage f = onNewEvent ["message"] $ \evt ->
  -- I could just pattern match here, as I did in other places and simply
  -- expect the Zulip API not to give us correct responses, but I think
  -- this is more reasonable.
  maybe (return ()) f (eventMessage evt)

-- Higher-level conduit interface:
-------------------------------------------------------------------------------

-- |
-- A sink representation of the zulip messaging API, takes a tuple with the
-- arguments for 'sendMessage' and sends it
sinkZulipMessages :: Sink (Text, [Text], Text, Text) ZulipM ()
sinkZulipMessages = loop
  where loop = await >>= maybe (return ())
                               (\(w, x, y, z) -> do
                                    void $ lift $ sendMessage w x y z
                                    loop)

-- |
-- Creates a conduit 'Source' of zulip events
sourceZulipEvents :: Int      -- ^ The size of the event buffer
                  -> [Text] -- ^ A list of event types to subscribe to
                  -> Source ZulipM Event
sourceZulipEvents bufSize evts = gatherFrom bufSize $
    onNewEvent evts . zulipWriteTBQueueIO

-- |
-- Creates a conduit 'Source' of zulip messages
sourceZulipMessages :: Int -- ^ The size of the event buffer
                    -> Source ZulipM Message
sourceZulipMessages bufSize = gatherFrom bufSize $
    onNewMessage . zulipWriteTBQueueIO

-- Private functions:
-------------------------------------------------------------------------------

data Endpoint
  = EndpointMessages
  | EndpointRegister
  | EndpointEvents
  | EndpointProfile
  | EndpointSubscriptions
  | EndpointStreams
  deriving (Eq, Ord, Show)

-- |
-- Key-value pair abstraction for working with querystrings or form-data
type RequestData = [(Text, Text)]

-- |
-- Makes a request to some @Endpoint@ in the zulip API
zulipMakeRequest :: Endpoint -> Method -> RequestData -> ZulipM BL.ByteString
zulipMakeRequest e = zulipMakeRequest' (endpointSuffix e)

-- |
-- Makes a request to some untyped URL in the zulip API. Serializes the
-- data as a QueryString on GET requests and as form-data otherwise
zulipMakeRequest' :: Text -> Method -> RequestData -> ZulipM BL.ByteString
zulipMakeRequest' u m d = do
    z <- ask
    req  <- liftIO $ parseUrl (T.unpack (clientBaseUrl z <> u))
    req' <- prepareRequest d req m
    res  <- liftIO $ httpLbs req' { method = m } $ clientManager z
    return $ responseBody res

-- |
-- A helper for decoding a response in the Zulip monad
decodeResponse :: BL.ByteString -> ZulipM ZT.Response
decodeResponse b = do
  case eitherDecode' b of
    Right r -> if wasSuccessful r then return r
                                  else fail $ T.unpack $ responseMsg r
    Left err -> fail $ "decodeResponse: Unexpected response from the Zulip API: " ++ CL.unpack b ++ "\n"
                       ++ "Error was: " ++ err

-- |
-- Adds a QueryString or FormData body, represented by a list of tuples,
-- and authenticates the request, with the current zulip state's
-- credentials.
prepareRequest :: RequestData -> Request -> Method -> ZulipM Request
prepareRequest [] r _ = applyAuth r
prepareRequest d r m | m == methodGet =
    applyAuth $ setQueryString (map helper d) r
  where helper (k, v) = (encodeUtf8 k, Just $ encodeUtf8 v)
prepareRequest d r _ =
    applyAuth =<< formDataBody (map (uncurry partBS . second encodeUtf8) d) r

-- |
-- Adds authentication to a 'Request' with the configuration in the 'ZulipM'
-- monad
applyAuth :: Request -> ZulipM Request
applyAuth req = do
      ZulipOptions e k _ _ <- ask
      return $ applyBasicAuth (encodeUtf8 e) (encodeUtf8 k) req

-- |
-- Returns `True` if a response indicates success
wasSuccessful :: ZT.Response -> Bool
wasSuccessful = (== ResponseSuccess) . responseResult

-- |
-- Gets the suffix for some endpoint
endpointSuffix :: Endpoint -> Text
endpointSuffix EndpointMessages      = "/messages"
endpointSuffix EndpointEvents        = "/events"
endpointSuffix EndpointRegister      = "/register"
endpointSuffix EndpointProfile       = "/users/me"
endpointSuffix EndpointSubscriptions = "/users/me/subscriptions"
endpointSuffix EndpointStreams       = "/streams"

-- |
-- Lifted IO version of 'writeTBQueue'
zulipWriteTBQueueIO :: TBQueue a -> a -> ZulipM ()
zulipWriteTBQueueIO q x = lift $ atomically $ writeTBQueue q x
