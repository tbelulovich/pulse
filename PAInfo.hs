module Main(
  main
  ) where

import Pulse
import System.Environment

fieldHandler :: (t -> String) -> t -> IO ()
fieldHandler f s = do
  putStrLn $ f s

printUsage :: IO ()
printUsage = do
  putStrLn "painfo                 - show all sink info"
  putStrLn "painfo vol  <sink_id>  - show volume"
  putStrLn "painfo mute <sink_id>  - show mute status"
  putStrLn "painfo name <sink_id>  - show name of sink" 

nullHandler :: IO ()
nullHandler = mapM_ print =<< getSinks

commandHandler :: String -> Int -> IO ()
commandHandler c sid = do
  sink <- sinkByID sid
  let f = case c of
        "vol" -> show . vol
        "mute" -> show . mute
        "name" -> name
  fieldHandler f sink
  
dispatcher :: [String] -> IO ()
dispatcher args =
  case args of
    [] -> nullHandler
    [c,sid] -> commandHandler c (read sid)
    _ -> printUsage
    
main :: IO ()
main = dispatcher =<< getArgs
