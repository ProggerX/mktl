module Mktl.Parser where

import Control.Applicative hiding (many, (<|>))
import Data.Functor
import Data.Text
import Data.Time
import Text.Parsec
import Text.Parsec.Text

type Timeline = [Event]

data Event
  = Event
  { start :: Day
  , end :: Day
  , desc :: Text
  }
  deriving (Show)

parseTimeline :: Parser Timeline
parseTimeline = many parseEvent

parseEvent :: Parser Event
parseEvent =
  try
    ( do
        s <- manyTill (digit <|> char '-') space
        e <- manyTill (digit <|> char '-') space
        t <- pack <$> manyTill anyChar ((some endOfLine $> ()) <|> eof)
        pure $ Event (read s) (read e) t
    )
    <|> do
      d <- manyTill anyChar space
      t <- pack <$> manyTill anyChar ((some endOfLine $> ()) <|> eof)
      pure $ Event (read d) (read d) t
