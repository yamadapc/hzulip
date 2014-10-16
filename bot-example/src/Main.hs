{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
import Control.Concurrent (forkIO)
import Control.Monad (unless, liftM, void)
import Data.List (isPrefixOf, stripPrefix)
import Mueval.ArgsParse (Options(..))
import Mueval.Context (defaultModules, defaultPackages)
import Mueval.Interpreter (interpreter)
import Mueval.Parallel (watchDog)
import Language.Haskell.Interpreter (runInterpreter, InterpreterError)
import System.Environment (getEnv)
import System.Process (readProcess)

import HZulip

main :: IO ()
main = do
    user <- getEnv "ZULIP_USER"
    key <- getEnv "ZULIP_KEY"

    let zulip = newZulip user key

    putStrLn "Listening for events"
    _ <- forkIO $ onNewEvent zulip ["message"] printEvent
    onNewMessage zulip $ \msg -> do
        tid <- forkIO $ executeCommand zulip msg
        watchDog 20 tid

printEvent :: EventCallback
printEvent (Event t i (Just m)) = putStrLn $ "" ++ 
                                             t ++ ": " ++
                                             "(ID: " ++ show i ++ ") '" ++
                                             messageContent m ++ "'"
printEvent _ = return ()

executeCommand :: ZulipClient -> MessageCallback
executeCommand z msg = do
    let e = userEmail $ messageSender msg
        tp = messageSubject msg
        cm = messageContent msg

    unless (e == clientEmail z) $ executeCommand' cm >>=
                                  \case
                                     Just r -> do
                                       putStrLn $ "Sending message: " ++ r
                                       let rec = messageDisplayRecipient msg
                                       void $ case rec of
                                           Left s ->
                                               sendStreamMessage z s tp r
                                           Right us ->
                                               let es = map userEmail us in
                                               sendPrivateMessage z es r
                                     Nothing -> return ()

executeCommand' :: String -> IO (Maybe String)
executeCommand' cm | Just expr <- stripPrefix "@eval haskell " cm = do
                        putStrLn $ "Evaluating haskell: " ++ expr
                        liftM Just $ executeEval expr
                   | Just expr <- stripPrefix "@eval javascript " cm = do
                        putStrLn $ "Evaluating javascript: " ++ expr
                        liftM Just $ executeJavascript expr
                   | otherwise = return Nothing

executeEval :: String -> IO String
executeEval expr = mueval expr >>= \case
    Left err -> do putStrLn $ "Error: " ++ show err
                   return $ "Error: Couldn't evaluate your expression\n" ++
                            "Sorry for the lack of a decent error message"
    Right (_, _, val) -> return $ take 50 val

executeJavascript :: String -> IO String
executeJavascript expr = readProcess "nodejs" ["external-evaluators/eval.js", expr] ""

fixLineBreaks :: String -> String
fixLineBreaks = replace "\\8217" "\8217" .
                replace "\\t" "\t" .
                replace "\\n" "\n"

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace a b s@(x:xs) | a `isPrefixOf` s = b ++ replace a b (drop (length a) s)
                     | otherwise = x : replace a b xs

muevalOptions :: Options
muevalOptions = Options { expression = ""
                        , modules = Just defaultModules
                        , trustedPackages = defaultPackages 
                        , timeLimit = 5
                        , user = ""
                        , loadFile = ""
                        , printType = False
                        , namedExtensions = []
                        , extensions = False
                        , help = False
                        , noImports = True
                        , rLimits = False
                        , packageTrust = False
                        }

mueval :: String -> IO (Either InterpreterError (String, String, String))
mueval expr = runInterpreter (interpreter muevalOptions { expression = expr
                                                        })
