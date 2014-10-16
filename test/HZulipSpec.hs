module HZulipSpec where

import System.Environment (getEnv)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec

import HZulip

spec :: Spec
spec = do
    -- TODO - stub IO
    let user = unsafePerformIO $ getEnv "ZULIP_USER"
        key = unsafePerformIO $ getEnv "ZULIP_KEY"
        z = newZulip user key

    describe "getSubscriptions" $
        it "returns the bot's current subscriptions" $ do
            subscriptions <- getSubscriptions z
            subscriptions `shouldBe` ["haskell"]
