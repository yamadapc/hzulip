{-# LANGUAGE OverloadedStrings #-}
module HZulip where

import Control.Lens
import Control.Monad
import Data.Aeson.Lens
import qualified Data.Text as DT (unpack)
import qualified Data.ByteString.Char8 as BS (pack)
import Network.Wreq

data ZulipClient = ZulipClient { email   :: String
                               , apiKey  :: String
                               , baseUrl :: String
                               }

data Message = Message { mType   :: String
                       , content :: String
                       , to      :: [String]
                       , subject :: Maybe String
                       , id      :: Maybe String
                       }

wasSent :: Message -> Bool
wasSent (Message _ _ _ _ (Just _)) = True
wasSent (Message _ _ _ _ Nothing)  = False

defaultBaseUrl :: String
defaultBaseUrl = "https://api.zulip.com/v1"

messagesUrl :: ZulipClient -> String
messagesUrl = (++ "/messages") . baseUrl

reqOptions :: ZulipClient -> Options
reqOptions (ZulipClient e k _) = defaults & auth .~ basicAuth (BS.pack e) (BS.pack k)

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

newZulip :: String -> String -> Maybe String -> ZulipClient
newZulip e k (Just u) = ZulipClient e k u
newZulip e k Nothing  = ZulipClient e k defaultBaseUrl

-- POST https://api.zulip.com/v1/messages
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

-- TODO:
-- POST https://api.zulip.com/v1/register
-- GET https://api.zulip.com/v1/events
