module Main where

import Pulse
import System.Environment
import Control.Applicative
import System.Process
import Control.Monad

printUsage :: IO ()
printUsage = do
  putStrLn "papipe loop <source> <sink>"
  putStrLn "papipe combine <out> <in_1> <in_2>"

buildCommand :: String -> Sink -> Sink -> String
buildCommand cname s t =
  "pactl load-module module-combine-sink sink_name="
  ++ cname
  ++ " slaves="
  ++ (name s)
  ++ ","
  ++ (name t)

loopcommand :: String -> String -> String
loopcommand source sink =
  "load-module module-loopback source="
    ++ source
    ++ " sink="
    ++ sink

loopback :: IO ()
loopback = do
  [_,source,sink] <- getArgs
  void $ runCommand $ loopcommand source sink

  
combine :: IO ()
combine = do
  args <-  tail <$> getArgs
  [sink1, sink2] <-  mapM (sinkByID . read) (tail args)  
  let sink3 = head args
  void $ runCommand $ buildCommand sink3 sink1 sink2


main :: IO ()
main = do
  args <- getArgs
  case length args of
    3 -> loopback
    4 -> combine
    _ -> printUsage

