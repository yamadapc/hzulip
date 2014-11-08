{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module HZulipSpec where

import Control.Concurrent.Async (async)
import Control.Monad (void)
import Control.Monad.Trans (lift)
import Text.RawString.QQ
import Test.Hspec
import Web.Scotty

import Web.HZulip

spec :: Spec
spec = beforeAll withServer $ do
    let user = "ZULIP_USER"
        key = "ZULIP_KEY"

    describe "getStreams" $
        it "returns all the public zulip streams" $ do
            z <- zulipOptions user key
            withZulip z { clientBaseUrl = "http://localhost:3000" } $ do
                streams <- getStreams
                lift $ streams `shouldBe` ["social", "jobs"]

    describe "addSubscriptions" $
        it "doesn't throw an exception" $ do
            z <- zulipOptions user key
            withZulip z { clientBaseUrl = "http://localhost:3000" } $ do
                addSubscriptions ["clojure"]

    describe "getSubscriptions" $
        it "returns the bot's current subscriptions" $ do
            z <- zulipOptions user key
            withZulip z { clientBaseUrl = "http://localhost:3000" } $ do
                subscriptions <- getSubscriptions
                lift $ subscriptions `shouldBe` ["social", "haskell"]

    describe "removeSubscriptions" $
        it "doesn't throw an exception" $ do
            z <- zulipOptions user key
            withZulip z { clientBaseUrl = "http://localhost:3000" } $ do
                removeSubscriptions ["here"]

    describe "getStreamSubscribers" $
        it "returns a stream's subscribers" $ do
            z <- zulipOptions user key
            withZulip z { clientBaseUrl = "http://localhost:3000" } $ do
                subs <- getStreamSubscribers "haskell"
                lift $ subs `shouldBe` ["someone", "here"]

withServer :: IO ()
withServer = void $ async $ scotty 3000 mockServer

mockServer :: ScottyM ()
mockServer = do
    get   "/streams"                 $ rawJson exampleStreamsResponse
    get   "/streams/haskell/members" $ rawJson exampleMembersResponse
    get   "/users/me/subscriptions"  $ rawJson exampleSubscriptionsResponse
    post  "/users/me/subscriptions"  $ return ()
    patch "/users/me/subscriptions"  $ return ()
  where rawJson t = raw t >>
                    setHeader "Content-Type" "application/json; charset=utf-8"
        exampleSubscriptionsResponse = [r|{
  "msg":"",
  "result":"success",
  "subscriptions":[
    { "name":"social",
      "subscribers":["stein@dropbox.com","kenp@dropbox.com"],
      "invite_only":false,
      "color":"#76ce90",
      "audible_notifications":true,
      "description":"Topics of general interest (e.g. news, weather, etc.)",
      "email_address":"social+34a76ad3c436a33cc99f7f0e9039820c@streams.staging.zulip.com",
      "in_home_view":true
    },
    { "desktop_notifications":true,
      "stream_id":24807,
      "name":"haskell",
      "subscribers":["stein@dropbox.com","kenp@dropbox.com"],
      "invite_only":false,
      "color":"#76ce90",
      "audible_notifications":true,
      "description":"World domination with functional programming",
      "email_address":"social+34a76ad3c436a33cc99f7f0e9039820c@streams.staging.zulip.com",
      "in_home_view":true
    }
  ]
}|]
        exampleMembersResponse = [r|{
  "msg": "",
  "result": "success",
  "subscribers": ["someone", "here"]
}|]
        exampleStreamsResponse = [r|{
  "msg": "",
  "result": "success",
  "streams": [ { "name": "social" }, { "name": "jobs" } ]
}|]
