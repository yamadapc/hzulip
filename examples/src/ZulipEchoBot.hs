module Main
  where

import Control.Concurrent.Async.Lifted (async)
import Control.Exception (SomeException)
import Control.Monad (void, when)
import Control.Monad.Catch (catchAll)
import Data.List (intercalate, isPrefixOf)
import System.Environment (getEnv)

import Web.HZulip

main :: IO ()
main = withZulipEnv $ do
    lift $ putStrLn "Subscribing to all streams..."
    void addAllSubscriptions

    lift $ putStrLn "Echoing..."
    catchAll startEchoer onZulipError

startEchoer :: ZulipM ()
startEchoer = onNewMessage $ \msg -> do
    nr <- nonRecursive msg
    let c = messageContent msg

    when (nr && "echo " `isPrefixOf` c) $ void $ async $ do
        r <- case messageType msg of
            "stream" ->
                let Left stream = messageDisplayRecipient msg
                    topic = messageSubject msg
                  in sendStreamMessage stream topic c >> return topic
            "private" ->
                let Right users = messageDisplayRecipient msg
                    recipients = map userEmail users
                  in sendPrivateMessage recipients c >>
                     return (intercalate ", " recipients)
            t -> fail $ "Unrecognized message type " ++ t
        lift $ putStrLn $ "Echoed " ++ c ++ " to " ++ r

onZulipError :: SomeException -> ZulipM ()
onZulipError ex = lift $ putStrLn "Zulip Client errored:" >> print ex

nonRecursive :: Message -> ZulipM Bool
nonRecursive msg = do
    z <- ask
    return $ clientEmail z /= userEmail (messageSender msg)

withZulipEnv :: ZulipM a -> IO a
withZulipEnv action = do
    user <- getEnv "ZULIP_USER"
    key  <- getEnv "ZULIP_KEY"
    withZulipCreds user key action
