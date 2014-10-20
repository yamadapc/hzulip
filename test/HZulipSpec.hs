module HZulipSpec where

import System.Environment (getEnv)
import Test.Hspec

import HZulip

spec :: Spec
spec = do
    -- TODO - stub IO
    describe "getSubscriptions" $
        it "returns the bot's current subscriptions" $ do
            user <- getEnv "ZULIP_USER"
            key <- getEnv "ZULIP_KEY"
            z <- newZulip user key

            subscriptions <- getSubscriptions z
            subscriptions `shouldBe` ["haskell"]
