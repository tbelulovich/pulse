module Test where
import Pulse

import Control.Applicative
import Text.Parsec hiding (many)

test = do
  r <- readFile "sink-small"
  return $ parse parseSink "" r

testMany = do
  r <- readFile "sink-sample"
  return $ either (const []) id (parse (many parseSink) "" r)
