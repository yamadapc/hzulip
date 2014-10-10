{-# LANGUAGE OverloadedStrings #-}
module HZulip ( Message(..)
              , ZulipClient(..)
              , defaultBaseUrl
              , getEvents
              , newZulip
              , registerQueue
              , sendMessage
              )
  where

import Control.Lens ((.~), (&), (^.))
import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.Text as T (pack)
import Network.Wreq
import qualified Network.Wreq.Types as WT (params)

import HZulip.Types as ZT

-- Public functions:
-------------------------------------------------------------------------------

-- |
-- Helper for creating a `ZulipClient` with the `baseUrl` set to
-- `defaultBaseUrl`
newZulip :: String -> String -> ZulipClient
newZulip e k = ZulipClient e k defaultBaseUrl

-- |
-- The default zulip API URL
defaultBaseUrl :: String
defaultBaseUrl = "https://api.zulip.com/v1"

-- |
-- This wraps `POST https://api.zulip.com/v1/messages` with a nicer root
-- API. Simpler helpers for each specific case of this somewhat overloaded
-- endpoint will also be provided in the future.
--
-- It takes the message `mtype`, `mrecipients`, `msubject` and `mcontent`
-- and returns the created message's `id` in the `IO` monad.
sendMessage :: ZulipClient -> String -> [String] -> String -> String -> IO String
sendMessage z mtype mrecipients msubject mcontent = do
    let form = [ "type"    := mtype
               , "content" := mcontent
               , "to"      := show mrecipients
               , "subject" := msubject
               ]

    r <- postWith (reqOptions z) (messagesUrl z) form >>= asJSON
    let body = r ^. responseBody

    if wasSuccessful body
        then let Just mid = responseMessageId body in return mid
        else fail $ responseMsg body

-- |
-- This registers a new event queue with the zulip API. It's a lower level
-- function, which shouldn't be used unless you know what you're doing. It
-- takes a `ZulipClient`, a list of names of the events you want to listen
-- for and whether you'd like for the content to be rendered in HTML format
-- (if you set the last parameter to `False` it will be kept as typed, in
-- markdown format)
registerQueue :: ZulipClient -> [String] -> Bool -> IO Queue
registerQueue z evTps mdn = do
    let form = [ "event_types"    := show evTps
               , "apply_markdown" := (if mdn then "true" else "false" :: String)
               ]

    r <- postWith (reqOptions z) (registerUrl z) form >>= asJSON
    let body = r ^. responseBody

    if wasSuccessful body
        then let Just qid = responseQueueId body
                 Just lid = responseLastEventId body in
             return $ Queue qid lid
        else fail $ responseMsg body

getEvents :: ZulipClient -> Queue -> Bool -> IO [Event]
getEvents z q b = do
    let opts = (reqOptions z) { WT.params = [ ("queue_id", T.pack $ queueId q)
                                            , ("last_event_id", T.pack $ show $
                                                                lastEventId q)
                                            , ("dont_block", if b then "true"
                                                             else "false")
                                            ]
    }

    r <- getWith opts (eventsUrl z) >>= asJSON
    let body = r ^. responseBody

    if wasSuccessful body
        then let Just evs = responseEvents body in return evs
        else fail $ responseMsg body

-- Private functions:
-------------------------------------------------------------------------------

-- |
-- Returns `True` if a response indicates success
wasSuccessful :: ZT.Response -> Bool
wasSuccessful = (== ResponseSuccess) . responseResult

-- |
-- Gets the endpoint for creating messages for a given `ZulipClient`
messagesUrl :: ZulipClient -> String
messagesUrl = (++ "/messages") . clientBaseUrl

-- |
-- Gets the endpoint for registering event queues for a given `ZulipClient`
registerUrl :: ZulipClient -> String
registerUrl = (++ "/register") . clientBaseUrl

-- |
-- Gets the endpoint for fetching events for a given `ZulipClient`
eventsUrl :: ZulipClient -> String
eventsUrl = (++ "/events") . clientBaseUrl

-- |
-- Constructs the `Wreq` HTTP request `Options` object for a `ZulipClient`
reqOptions :: ZulipClient -> Options
reqOptions (ZulipClient e k _) = defaults & auth .~ basicAuth (BS.pack e) (BS.pack k)
