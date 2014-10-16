{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module HZulipTypesSpec where

import Data.Aeson
import Test.Hspec
import Text.RawString.QQ

import HZulip.Types

spec :: Spec
spec = do
    describe "instance FromJSON Event" $
        it "parses correctly" $
            decode eventJSON `shouldBe` Just Event { eventId = 0
                                                   , eventMessage = Nothing
                                                   , eventType = "weird"
                                                   }

    describe "instance FromJSON User" $
        it "parses correctly" $
            decode userJSON `shouldBe` Just User { userId = 1234
                                                 , userFullName = "Joe Doe"
                                                 , userEmail = "asf@asdf.com"
                                                 , userDomain = ""
                                                 , userShortName = "Joe"
                                                 }

    describe "instance FromJSON Message" $ do
        it "parses correctly when display_recipient is a string" $
            decode messageJSON `shouldBe`
                Just Message { messageId = 12345678
                             , messageType = "stream"
                             , messageContent = "Something is rotten in the state of Denmark."
                             , messageAvatarUrl = "https://url/for/othello-bots/avatar"
                             , messageTimestamp = 1375978403
                             , messageDisplayRecipient = Left "Denmark"
                             , messageSender = User { userId = 13215
                                                    , userFullName = "Othello Bot"
                                                    , userEmail = "othello-bot@example.com"
                                                    , userDomain = "example.com"
                                                    , userShortName = "othello-bot"
                                                    }
                             , messageGravatarHash = "17d93357cca1e793739836ecbc7a9bf7"
                             , messageRecipientId = 12314
                             , messageClient = "website"
                             , messageSubjectLinks = []
                             , messageSubject = "Castle"
                             }

        it "parses correctly when display_recipient is a list of users" $
            decode messageJSONWeird `shouldBe`
                Just Message { messageId = 12345678
                             , messageType = "stream"
                             , messageContent = "Something is rotten in the state of Denmark."
                             , messageAvatarUrl = "https://url/for/othello-bots/avatar"
                             , messageTimestamp = 1375978403
                             , messageDisplayRecipient =
                                 Right [ User { userFullName = "Hamlet of Denmark"
                                              , userDomain =  "example.com"
                                              , userEmail = "hamlet@example.com"
                                              , userShortName = "hamlet"
                                              , userId = 31572
                                              }
                                       ]
                             , messageSender = User { userId = 13215
                                                    , userFullName = "Othello Bot"
                                                    , userEmail = "othello-bot@example.com"
                                                    , userDomain = "example.com"
                                                    , userShortName = "othello-bot"
                                                    }
                             , messageGravatarHash = "17d93357cca1e793739836ecbc7a9bf7"
                             , messageRecipientId = 12314
                             , messageClient = "website"
                             , messageSubjectLinks = []
                             , messageSubject = "Castle"
                             }

  where userJSON = [r|{
 "id": 1234,
 "full_name": "Joe Doe",
 "email": "asf@asdf.com",
 "domain": "",
 "short_name": "Joe"
                   }|]
        messageJSON = [r|{
 "avatar_url": "https://url/for/othello-bots/avatar",
 "timestamp": 1375978403,
 "display_recipient": "Denmark",
 "sender_id": 13215,
 "sender_full_name": "Othello Bot",
 "sender_email": "othello-bot@example.com",
 "sender_short_name": "othello-bot",
 "sender_domain": "example.com",
 "content": "Something is rotten in the state of Denmark.",
 "gravatar_hash": "17d93357cca1e793739836ecbc7a9bf7",
 "recipient_id": 12314,
 "client": "website",
 "subject_links": [],
 "subject": "Castle",
 "type": "stream",
 "id": 12345678
                      }|]
        messageJSONWeird = [r|{
 "avatar_url": "https://url/for/othello-bots/avatar",
 "timestamp": 1375978403,
 "sender_id": 13215,
 "sender_full_name": "Othello Bot",
 "sender_email": "othello-bot@example.com",
 "sender_short_name": "othello-bot",
 "sender_domain": "example.com",
 "content": "Something is rotten in the state of Denmark.",
 "gravatar_hash": "17d93357cca1e793739836ecbc7a9bf7",
 "recipient_id": 12314,
 "client": "website",
 "subject_links": [],
 "subject": "Castle",
 "type": "stream",
 "id": 12345678,
  "display_recipient": [
    {
      "full_name": "Hamlet of Denmark",
      "domain": "example.com",
      "email": "hamlet@example.com",
      "short_name": "hamlet",
      "id": 31572
    }
  ]
                           }|]
        eventJSON = [r|{
 "type": "weird",
 "id": 0
                    }|]
