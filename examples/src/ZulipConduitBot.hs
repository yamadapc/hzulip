{-# LANGUAGE OverloadedStrings #-}

module Main
  where

import Control.Exception (SomeException)
import Control.Monad (void, when)
import Control.Monad.Catch (catchAll)
import Data.Conduit (($$), ($=), Conduit, awaitForever, yield)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getEnv)

import Web.HZulip

main :: IO ()
main = withZulipEnv $ do
    lift $ putStrLn "Subscribing to all streams..."
    void addAllSubscriptions

    lift $ putStrLn "Echoing..."
    catchAll startEchoer onZulipError

startEchoer :: ZulipM ()
startEchoer = sourceZulipMessages 30 $= echoConduit $$ sinkZulipMessages

echoConduit :: Conduit Message ZulipM (Text, [Text], Text, Text)
echoConduit = loop
  where loop = awaitForever processMessage >> loop
        processMessage msg = do
            nr <- lift $ nonRecursive msg
            let c = messageContent msg
            lift $ lift $ putStr "here"
            when (nr && "echo " `T.isPrefixOf` c) $
                let c' = T.drop 5 c in case messageType msg of
                    "stream" ->
                        let Left stream = messageDisplayRecipient msg
                            topic = messageSubject msg
                          in yield ("stream", [stream], topic, c')
                    "private" ->
                        let Right users = messageDisplayRecipient msg
                            recipients = map userEmail users
                          in lift (lift $ putStr "here") >>
                             yield ("private", recipients, "", c')
                    _ -> return ()

onZulipError :: SomeException -> ZulipM ()
onZulipError ex = lift $ putStrLn "Zulip Client errored:" >> print ex

nonRecursive :: Message -> ZulipM Bool
nonRecursive msg = do
    z <- ask
    return $ clientEmail z /= userEmail (messageSender msg)

withZulipEnv :: ZulipM a -> IO a
withZulipEnv action = do
    user <- fmap T.pack $ getEnv "ZULIP_USER"
    key  <- fmap T.pack $ getEnv "ZULIP_KEY"
    withZulipCreds user key action
