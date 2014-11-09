module Main
  where

import Control.Exception (SomeException)
import Control.Monad.Catch (catchAll)
import Control.Monad.Trans (lift)
import System.Environment (getEnv)

import Web.HZulip
import Web.HZulip.Types

main :: IO ()
main = withZulipEnv $ do
    lift $ putStrLn "Subscribing to all streams..."
    addAllSubscriptions

    lift $ putStrLn "Logging:"
    catchAll startLogger onZulipError

startLogger :: ZulipM ()
startLogger = onNewEvent eventTypes $ \evt ->
    lift $ print evt

onZulipError :: SomeException -> ZulipM ()
onZulipError ex = lift $ putStrLn "Zulip Client errored:" >> print ex

withZulipEnv :: ZulipM a -> IO a
withZulipEnv action = do
    user <- getEnv "ZULIP_USER"
    key  <- getEnv "ZULIP_KEY"
    withZulipCreds user key action
