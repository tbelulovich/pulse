{-#LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module Pulse
       ( getSinks
         
       , Sink
       , sink_id
       , mute
       , vol
       , name
       )
       where 
import Control.Applicative
import Control.Monad

import Text.Parsec hiding (many, (<|>))
import System.Process

data Sink = Sink {sink_id :: Int
                 ,mute :: Bool
                 ,vol :: Int
                 ,name :: String
                 } deriving (Show, Eq)

until p end = do
  x <- optionMaybe $ try end
  case x of
    Nothing -> p
    Just y -> return y

parseField f = do
  manyTill anyChar (try $ string (f ++ ": "))
  manyTill anyChar newline

-- | parseSink parses (part) of the output of pactl list sinks
parseSink :: Stream s m Char => ParsecT s u m Sink
parseSink = do
  string "Sink #"
  x <- manyTill digit space
  n <- parseField "Name"
  m <- parseField "Mute" 
  let mbool = case m of
        "yes" -> True
        "no" -> False
  manyTill anyChar (try $ string "Volume: ")
  many digit
  string ":"
  many space
  y <- many digit
  string "%"
  manyTill anyChar (try $ newline >> ((void newline) <|> eof))
  return $ Sink { sink_id = read x
                , mute = mbool
                , vol = read y
                , name = n
                }
    
-- | parses the output of pactl list sinks
getSinks :: IO [Sink]
getSinks = do
    sinks <- readProcess "pactl" ["list", "sinks"] ""
    return $ either (const []) id $ parse (many parseSink) "" sinks
