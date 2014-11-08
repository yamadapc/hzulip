module HZulipSpec where

import Control.Monad.IO.Class
import System.Environment (getEnv)
import Test.Hspec

import Web.HZulip

spec :: Spec
spec = do
    -- TODO - stub IO
    describe "getSubscriptions" $
        it "returns the bot's current subscriptions" $ do
            user <- getEnv "ZULIP_USER"
            key <- getEnv "ZULIP_KEY"
            z <- zulipOptions user key

            withZulip z $ do
                subscriptions <- getSubscriptions
                liftIO $ subscriptions `shouldBe` ["haskell"]
