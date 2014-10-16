{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
import Control.Concurrent (forkIO)
import Control.Monad (unless, liftM)
import Data.List (isPrefixOf, stripPrefix)
import Mueval.ArgsParse (Options(..))
import Mueval.Context (defaultModules, defaultPackages)
import Mueval.Interpreter (interpreter)
import Language.Haskell.Interpreter (runInterpreter, InterpreterError)

import System.Environment (getEnv)

import HZulip

main :: IO ()
main = do
    user <- getEnv "ZULIP_USER"
    key <- getEnv "ZULIP_KEY"

    let zulip = newZulip user key

    putStrLn "Listening for events"
    _ <- forkIO $ onNewEvent zulip ["message"] printEvent
    onNewMessage zulip (executeCommand zulip)

printEvent :: EventCallback
printEvent (Event t i (Just m)) = putStrLn $ "" ++ 
                                             t ++ ": " ++
                                             "(ID: " ++ show i ++ ") '" ++
                                             messageContent m ++ "'"
printEvent _ = return ()

executeCommand :: ZulipClient -> MessageCallback
executeCommand z msg = do
    let e = userEmail $ messageSender msg
        cm = messageContent msg

    unless (e == clientEmail z) $ executeCommand' cm >>=
                                  \case
                                     Just r -> do
                                       putStrLn $ "Sending message: " ++ r
                                       sendPrivateMessage z [e] r
                                       return ()
                                     Nothing -> return ()

executeCommand' :: String -> IO (Maybe String)
executeCommand' cm | Just expr <- stripPrefix ":t " cm = do
                        putStrLn $ "Evaluating type: " ++ expr
                        liftM Just $ executeEvalType expr
                   | Just expr <- stripPrefix ":e " cm = do
                        putStrLn $ "Evaluating: " ++ expr
                        liftM Just $ executeEval expr
                   | otherwise = return Nothing

executeEval :: String -> IO String
executeEval expr = mueval expr >>= \case
    Left err -> do putStrLn $ "Error: " ++ show err
                   return $ "Error: Couldn't evaluate your expression\n" ++
                            "Sorry for the lack of a decent error message"
    Right (_, _, val) -> return $ take 50 val

executeEvalType :: String -> IO String
executeEvalType expr = mueval expr >>= \case
    Left err -> do putStrLn $ "Error: " ++ show err
                   return $ "Error: Couldn't evaluate your expression\n" ++
                            "Sorry for the lack of a decent error message"
    Right (e, et, _) -> return $ e ++ "" ++ et

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
