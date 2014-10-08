{-# LANGUAGE OverloadedStrings #-}
module HZulip ( Message(..)
              , ZulipClient(..)
              , defaultBaseUrl
              , newZulip
              , sendMessage
              , wasSent
              )
  where

import Control.Lens
import Control.Monad
import Data.Aeson.Lens
import qualified Data.Text as DT (unpack)
import qualified Data.ByteString.Char8 as BS (pack)
import Network.Wreq

-- Public types and functions:
-------------------------------------------------------------------------------

-- |Represents a Zulip API client
data ZulipClient = ZulipClient { email   :: String
                               , apiKey  :: String
                               , baseUrl :: String
                               }

-- |Helper for creating a `ZulipClient` with the `baseUrl` set to
-- `defaultBaseUrl`
newZulip :: String -> String -> ZulipClient
newZulip e k = ZulipClient e k defaultBaseUrl

-- |Represents a Zulip Message
data Message = Message { mType   :: String
                       , content :: String
                       , to      :: [String]
                       , subject :: Maybe String
                       , id      :: Maybe String
                       }

-- |A helper for determining whether a `Message` was sent. This is the same
-- as doing something like `Data.Maybe.isJust $ HZulip.id msg`.
wasSent :: Message -> Bool
wasSent (Message _ _ _ _ (Just _)) = True
wasSent (Message _ _ _ _ Nothing)  = False

-- !The default zulip API URL
defaultBaseUrl :: String
defaultBaseUrl = "https://api.zulip.com/v1"

-- |This wraps `POST https://api.zulip.com/v1/messages` with a nicer root
-- API. Simpler helpers for each specific case of this somewhat overloaded
-- endpoint will also be provided in the future.
sendMessage :: ZulipClient -> Message -> IO Message
sendMessage z m = do
    response <- postWith (reqOptions z) (messagesUrl z) (formFromMessage m)

    let body   = response ^. responseBody
        result = body ^? key "result"

    case result of
      Just "success" ->
        let mid = body ^? key "id" . _String in
          return $ m { HZulip.id = liftM DT.unpack mid }
      Just "error" ->
        case body ^? key "msg" ._String of
          Just msg -> fail $ DT.unpack msg
          Nothing  -> fatalError response
      _ -> fatalError response
  where fatalError r = error $ "The Zulip API responded with: " ++ show r

-- TODO
-- Wrappers for:
-- - POST https://api.zulip.com/v1/register
-- - GET https://api.zulip.com/v1/events

-- Private types and functions:
-------------------------------------------------------------------------------

-- |Gets the endpoint for creating messages for a given `ZulipClient`.
messagesUrl :: ZulipClient -> String
messagesUrl = (++ "/messages") . baseUrl

-- |Constructs the `Wreq` HTTP request `Options` object for
-- a `ZulipClient`.
reqOptions :: ZulipClient -> Options
reqOptions (ZulipClient e k _) = defaults & auth .~ basicAuth (BS.pack e) (BS.pack k)

-- |Constructs a list of form-data request's `FormParam` objects from
-- a `Message` to post to zulip's API.
formFromMessage :: Message -> [FormParam]
formFromMessage (Message t c r ms _) = [ "type"    := t
                                       , "content" := c
                                       , "to"      := r'
                                       ] ++ case ms of
                                                Just s -> [ "subject" := s ]
                                                _      -> []
  where r' = if t == "private"
                 then show r -- JSON Arrays and Haskell Arrays should be equal
                 else head r -- in this case

