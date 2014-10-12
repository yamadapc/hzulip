import System.Environment (getEnv)

import HZulip

main :: IO ()
main = do
    user <- getEnv "ZULIP_USER"
    key <- getEnv "ZULIP_KEY"

    let zulip = newZulip user key

    putStrLn "Listening for events"
    onNewEvent zulip False printEvent

printEvent :: EventCallback
printEvent (Event t i (Just m)) = putStrLn $ t ++ " - " ++
                                             show i ++ " - " ++
                                             messageContent m
printEvent _ = return ()
