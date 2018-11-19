module Main
  where

import Control.Exception (SomeException)
import Control.Monad (void)
import Control.Monad.Catch (catchAll)
import qualified Data.Text as T
import System.Environment (getEnv)

import Web.HZulip

main :: IO ()
main = withZulipEnv $ do
    lift $ putStrLn "Subscribing to all streams..."
    void addAllSubscriptions

    lift $ putStrLn "Logging:"
    catchAll startLogger onZulipError

startLogger :: ZulipM ()
startLogger = onNewEvent eventTypes $ \evt ->
    lift $ print evt

onZulipError :: SomeException -> ZulipM ()
onZulipError ex = lift $ putStrLn "Zulip Client errored:" >> print ex

withZulipEnv :: ZulipM a -> IO a
withZulipEnv action = do
    user <- fmap T.pack $ getEnv "ZULIP_USER"
    key  <- fmap T.pack $ getEnv "ZULIP_KEY"
    withZulipCreds user key action
