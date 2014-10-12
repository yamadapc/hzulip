{-# LANGUAGE OverloadedStrings #-}

module HZulip.Types where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (mzero)
import Data.Aeson

-- |
-- Represents a Zulip API client
data ZulipClient = ZulipClient { clientEmail   :: String
                               , clientApiKey  :: String
                               , clientBaseUrl :: String
                               }

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

instance FromJSON Response where
    parseJSON (Object o) = Response <$>
                           o .:  "result"        <*>
                           o .:  "msg"           <*>
                           o .:? "id"            <*>
                           o .:? "queue_id"      <*>
                           o .:? "last_event_id" <*>
                           o .:? "events"
    parseJSON _ = mzero

-- |
-- Represnts a response result, this is just so result Strings aren't
-- modeled in memory
data ResponseResult = ResponseError | ResponseSuccess
  deriving(Eq, Show, Ord)


instance FromJSON ResponseResult where
    parseJSON (String "success") = pure ResponseSuccess
    parseJSON _                  = pure ResponseError

-- |
-- Represents zulip events
data Event = Event { eventType    :: String
                   , eventId      :: Int
                   , eventMessage :: Maybe Message
                   }

instance FromJSON Event where
    parseJSON (Object o) = Event <$>
                           o .: "type"     <*>
                           o .:  "id"      <*>
                           o .:? "message"
    parseJSON _ = mzero

-- |
-- Represents a Zulip Message
data Message = Message { messageId               :: Int
                       , messageType             :: String
                       , messageContent          :: String

                       , messageAvatarUrl        :: String
                       , messageTimestamp        :: Int

                       -- See the comment on the `FromJSON Message` instance.
                       -- , messageDisplayRecipient :: Either String [User]

                       , messageSender           :: User

                       , messageGravatarHash     :: String

                       , messageRecipientId      :: Int
                       , messageClient           :: String
                       , messageSubjectLinks     :: [String]
                       , messageSubject          :: String
                       }

instance FromJSON Message where
    parseJSON (Object o) = Message <$>
                           o .: "id"                <*>
                           o .: "type"              <*>
                           o .: "content"           <*>

                           o .: "avatar_url"        <*>
                           o .: "timestamp"         <*>

                           -- I don't know if there's currently a way to
                           -- parse `Either` types using `Data.Aeson`.
                           -- o .: "display_recipient" <*>

                           (User <$>
                             o .: "sender_id"         <*>
                             o .: "sender_full_name"  <*>
                             o .: "sender_domain"     <*>
                             o .: "sender_email"      <*>
                             o .: "sender_short_name"
                           ) <*>

                           o .: "gravatar_hash"     <*>

                           o .: "recipient_id"      <*>
                           o .: "client"            <*>
                           o .: "subject_links"     <*>
                           o .: "subject"
    parseJSON _ = mzero

-- |
-- Represents a zulip user account - for both `display_recipient` and
-- `message_sender` representations
data User = User { userId        :: Int
                 , userFullName  :: String
                 , userEmail     :: String
                 , userDomain    :: String
                 , userShortName :: String
                 }

instance FromJSON User where
    parseJSON (Object o) = User <$>
                           o .: "id" <*>
                           o .: "full_name"  <*>
                           o .: "domain"     <*>
                           o .: "email"      <*>
                           o .: "short_name"
    parseJSON _ = mzero

-- |
-- Represents some event queue
data Queue = Queue { queueId     :: String
                   , lastEventId :: Int
                   }

-- |
-- The root type for Event callbacks
type EventCallback = Event -> IO ()

-- |
-- Type for message callbacks
type MessageCallback = Message -> IO ()
