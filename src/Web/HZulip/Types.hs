{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Web.HZulip.Types where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (mzero)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson.Types (FromJSON(..), Parser, Value(..), (.:?), (.:))
import Data.Text (Text)
import Network.HTTP.Client (Manager)

-- |
-- The Monad in which Zulip API actions happen in. This is a 'ReaderT'
-- alias, so it's also a instance of 'MonadTrans', 'MonadIO' etc.
type ZulipM = ReaderT ZulipOptions IO

-- |
-- Represents a Zulip API client
data ZulipOptions = ZulipOptions { clientEmail   :: Text
                                 , clientApiKey  :: Text
                                 , clientBaseUrl :: Text
                                 , clientManager :: Manager
                                 }

instance Show ZulipOptions where
    show ZulipOptions{..} = "ZulipOptions { " ++ show clientEmail  ++
                            "\n            , " ++ show clientApiKey ++
                            "\n            , " ++ show clientBaseUrl ++
                            "\n            , Manager {...}\n}"

-- |
-- The internal response representation for top-down parsing of Zulip API
-- JSON responses
data Response = Response { responseResult      :: ResponseResult
                         , responseMsg         :: Text

                         , responseMessageId   :: Maybe Int
                         , responseQueueId     :: Maybe Text
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
data Event = Event { eventType    :: Text
                   , eventId      :: Int
                   , eventMessage :: Maybe Message
                   }
  deriving (Eq, Ord, Show)

-- |
-- Represents a Zulip Message
data Message = Message { messageId               :: Int
                       , messageType             :: Text
                       , messageContent          :: Text

                       , messageAvatarUrl        :: Text
                       , messageTimestamp        :: Int

                       -- See the comment on the `FromJSON Message` instance.
                       , messageDisplayRecipient :: Either Text [User]

                       , messageSender           :: User

                       , messageRecipientId      :: Int
                       , messageClient           :: Text
                       , messageSubjectLinks     :: [Text]
                       , messageSubject          :: Text
                       }
  deriving (Eq, Ord, Show)

-- |
-- Represents the current user's profile
data Profile = Profile { profileClientId    :: Text
                       , profileEmail       :: Text
                       , profileFullName    :: Text
                       , profileIsAdmin     :: Bool
                       , profileIsBot       :: Bool
                       , profileMaxMessagId :: Int
                       , profilePointer     :: Int
                       , profileShortName   :: Text
                       , profileUserId      :: Int
                       }
  deriving (Eq, Ord, Show)

-- |
-- Represents a zulip user account - for both `display_recipient` and
-- `message_sender` representations
data User = User { userId        :: Int
                 , userFullName  :: Text
                 , userEmail     :: Text
                 , userRealm     :: Maybe Text
                 -- ^ `display_recipient` doesn't have this, `message_sender` does
                 , userShortName :: Text
                 }
  deriving (Eq, Ord, Show)

-- |
-- Represents some event queue
data Queue = Queue { queueId     :: Text
                   , lastEventId :: Int
                   }
  deriving (Eq, Ord, Show)

-- |
-- The root type for Event callbacks
type EventCallback = Event -> ZulipM ()

-- |
-- Type for message callbacks
type MessageCallback = Message -> ZulipM ()

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
                             -- sender_realm_str was sender_domain in Zulip < 1.6,
                             -- see https://github.com/zulip/zulip/commit/b416587aabec1e4ccf679652b7f3d61a6788317a
                             o .:? "sender_realm_str" <*>
                             o .: "sender_short_name"
                           ) <*>

                           o .: "recipient_id"      <*>
                           o .: "client"            <*>
                           o .: "subject_links"     <*>
                           o .: "subject"
    parseJSON _ = mzero

instance FromJSON Profile where
    parseJSON (Object o) = Profile <$>
                           o .: "client_id"      <*>
                           o .: "email"          <*>
                           o .: "full_name"      <*>
                           o .: "is_admin"       <*>
                           o .: "is_bot"         <*>
                           o .: "max_message_id" <*>
                           o .: "pointer"        <*>
                           o .: "short_name"     <*>
                           o .: "user_id"
    parseJSON _ = mzero

instance FromJSON User where
    parseJSON (Object o) = User <$>
                           o .: "id" <*>
                           o .: "full_name"  <*>
                           o .: "email"      <*>
                           pure Nothing      <*>
                           o .: "short_name"
    parseJSON _ = mzero

parseDisplayRecipient :: Value -> Parser (Either Text [User])
parseDisplayRecipient v = case v of
    -- Matching explicitly here instead of using <|>
    -- improves aeson error messages.
    Array{} -> Right <$> parseJSON v
    _ -> Left <$> parseJSON v
