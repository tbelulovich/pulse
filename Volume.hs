module Main(
  main
  ) where

import Pulse
import Control.Applicative
import System.Environment

has :: Eq a => (t -> a) -> a -> t -> Bool
has f c x = c == f x

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead x = Just $ head x

pp :: Sink -> IO ()
pp s = do
  putStr $ if mute s
             then "<fc=#00FFFF>"
             else "<fc=#FF0000>"
  putStr $ show $ vol s
  putStrLn "</fc>"

main :: IO ()
main = do
  sink_num <- read <$> (!! 0) <$> getArgs
  sinks <- getSinks
  let selected = maybeHead $ filter (has sink_id sink_num) sinks
  case selected of
    Nothing -> error  $ "Found no sink with id " ++ (show sink_num)
    Just s -> pp $ s
  
