{-# LANGUAGE OverloadedStrings #-}
module HZulip.Types where

import Control.Applicative ((<$>), (<*>), (<|>), pure)
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Types
import Network.Wreq.Session

-- |
-- Represents a Zulip API client
data ZulipClient = ZulipClient { clientEmail   :: String
                               , clientApiKey  :: String
                               , clientBaseUrl :: String
                               , clientSession :: Session
                               }
  deriving (Show)

-- |
-- The internal response representation for top-down parsing of Zulip API
-- JSON responses
data Response = Response { responseResult      :: ResponseResult
                         , responseMsg         :: String

                         , responseMessageId   :: Maybe Int
                         , responseQueueId     :: Maybe String
                         , responseLastEventId :: Maybe Int

                         , responseEvents      :: Maybe [Event]
                         }
  deriving (Eq, Ord, Show)

-- |
-- Represnts a response result, this is just so result Strings aren't
-- modeled in memory
data ResponseResult = ResponseError | ResponseSuccess
  deriving(Eq, Show, Ord)

-- |
-- Represents zulip events
data Event = Event { eventType    :: String
                   , eventId      :: Int
                   , eventMessage :: Maybe Message
                   }
  deriving (Eq, Ord, Show)

-- |
-- Represents a Zulip Message
data Message = Message { messageId               :: Int
                       , messageType             :: String
                       , messageContent          :: String

                       , messageAvatarUrl        :: String
                       , messageTimestamp        :: Int

                       -- See the comment on the `FromJSON Message` instance.
                       , messageDisplayRecipient :: Either String [User]

                       , messageSender           :: User

                       , messageGravatarHash     :: String

                       , messageRecipientId      :: Int
                       , messageClient           :: String
                       , messageSubjectLinks     :: [String]
                       , messageSubject          :: String
                       }
  deriving (Eq, Ord, Show)

-- |
-- Represents a zulip user account - for both `display_recipient` and
-- `message_sender` representations
data User = User { userId        :: Int
                 , userFullName  :: String
                 , userEmail     :: String
                 , userDomain    :: String
                 , userShortName :: String
                 }
  deriving (Eq, Ord, Show)

-- |
-- Represents some event queue
data Queue = Queue { queueId     :: String
                   , lastEventId :: Int
                   }
  deriving (Eq, Ord, Show)

-- |
-- The root type for Event callbacks
type EventCallback = Event -> IO ()

-- |
-- Type for message callbacks
type MessageCallback = Message -> IO ()

instance FromJSON Response where
    parseJSON (Object o) = Response <$>
                           o .:  "result"        <*>
                           o .:  "msg"           <*>
                           o .:? "id"            <*>
                           o .:? "queue_id"      <*>
                           o .:? "last_event_id" <*>
                           o .:? "events"
    parseJSON _ = mzero

instance FromJSON ResponseResult where
    parseJSON (String "success") = pure ResponseSuccess
    parseJSON _                  = pure ResponseError

instance FromJSON Event where
    parseJSON (Object o) = Event <$>
                           o .: "type"     <*>
                           o .:  "id"      <*>
                           o .:? "message"
    parseJSON _ = mzero

instance FromJSON Message where
    parseJSON (Object o) = Message <$>
                           o .: "id"                <*>
                           o .: "type"              <*>
                           o .: "content"           <*>

                           o .: "avatar_url"        <*>
                           o .: "timestamp"         <*>

                           (parseDisplayRecipient =<<
                             o .: "display_recipient") <*>

                           (User <$>
                             o .: "sender_id"         <*>
                             o .: "sender_full_name"  <*>
                             o .: "sender_email"      <*>
                             o .: "sender_domain"     <*>
                             o .: "sender_short_name"
                           ) <*>

                           o .: "gravatar_hash"     <*>

                           o .: "recipient_id"      <*>
                           o .: "client"            <*>
                           o .: "subject_links"     <*>
                           o .: "subject"
    parseJSON _ = mzero

instance FromJSON User where
    parseJSON (Object o) = User <$>
                           o .: "id" <*>
                           o .: "full_name"  <*>
                           o .: "email"      <*>
                           o .: "domain"     <*>
                           o .: "short_name"
    parseJSON _ = mzero

parseDisplayRecipient :: Value -> Parser (Either String [User])
parseDisplayRecipient v = Right <$> parseJSON v <|>
                          Left <$> parseJSON v
