-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License along
-- with this program; if not, write to the Free Software Foundation, Inc.,
-- 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
--
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent.Async.Lifted (async, wait)
import Control.Exception.Lifted (SomeException(..), handle)
import Data.List (find)
import Data.List.Split (chunksOf)
import Data.Text (Text)
import qualified Data.Text as T
import System.Console.ANSI (Color(..), ColorIntensity(..), ConsoleIntensity(..),
                            ConsoleLayer(..), SGR(..), setSGR)
import System.Console.Terminal.Size (Window(..), size)
import System.Environment (getEnv)
import Text.Printf (printf)
import Web.HZulip

main :: IO ()
main = withZulipEnv $ do
    lift $ logInfo "Subscribing to all streams..."

    streamNames <- getStreams
    addSubscriptions streamNames
    lift $ logInfo "Subscribed to:"
    let streams = streamsFromNames streamNames
    lift $ printStreamTable streams

    start streams
  where start st = do
            end <- async $ onNewMessage (lift . printMessage st)
            lift $ logInfo "Listening for messages"
            handle
                (\e -> lift (print (e :: SomeException)) >> start st)
                (wait end)

withZulipEnv :: ZulipM a -> IO a
withZulipEnv action = do
    user <- fmap T.pack $ getEnv "ZULIP_USER"
    key  <- fmap T.pack $ getEnv "ZULIP_KEY"
    withZulipCreds user key action

printMessage :: [Stream] -> Message -> IO ()
printMessage ss msg = do
  let Left streamName = messageDisplayRecipient msg
      mstream = find (\s -> name s == streamName) ss

  case mstream of
      Just stream -> printStreamName stream
      Nothing -> putStr $ "<" ++ T.unpack streamName ++ ">"

  putStr $ " " ++ T.unpack (userEmail $ messageSender msg) ++ " said:\n"
  putStr $ T.unpack $ messageContent msg
  putStr "\n\n"

-- Stream headings
-------------------------------------------------------------------------------


data Stream = Stream { name :: Text
                     , color :: Color
                     }

streamsFromNames :: [Text] -> [Stream]
streamsFromNames = zipWith helper ([0..] :: [Int])
  where cs = [Black, Red, Green, Yellow, Blue, Magenta, Cyan]
        l = length cs
        helper i n = Stream n (cs !! (i `rem` l))

printStreamName :: Stream -> IO ()
printStreamName (Stream n c) = putStrSGR sgr ("<" ++ T.unpack n ++ ">")
  where sgr = [ SetColor Foreground Vivid c
              , SetConsoleIntensity BoldIntensity
              ]

printStreamTable :: [Stream] -> IO ()
printStreamTable ss = do
    Just (Window windowSize _) <- size :: IO (Maybe (Window Int))

    let biggestLen = maximum $ map (T.length . name) ss
        groupSize = windowSize `div` biggestLen

    mapM_ (printGroup biggestLen) $ chunksOf groupSize ss
  where printAligned m (Stream n c) = setSGR [SetColor Foreground Vivid c] >>
                                      printf ("%" ++ show m ++ "s  ") n >>
                                      resetSGR
        printGroup m g = putStr " " >>
                         mapM_ (printAligned m) g >>
                         putStr "\n"

-- Pretty logging
-------------------------------------------------------------------------------

logHeading :: Color -> IO ()
logHeading c = putStrSGR headingSGR ">> "
  where headingSGR = [ SetColor Foreground Vivid c
                     , SetConsoleIntensity BoldIntensity
                     ]

logInfo :: String -> IO ()
logInfo str = logHeading Blue >> putStrLn str

putStrSGR :: [SGR] -> String -> IO ()
putStrSGR sgr str = setSGR sgr >> putStr str >> resetSGR

resetSGR :: IO ()
resetSGR = setSGR []
