{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Bifoldable
import Data.Text.IO qualified as TIO
import Data.Time
import Mktl.Parser
import Mktl.Render
import System.Environment
import Text.Parsec

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

year :: Day -> Int
year = read . formatTime defaultTimeLocale "%Y"

main :: IO ()
main = do
  fp <- (!! 0) <$> getArgs
  d <- read . (!! 1) <$> getArgs :: IO Int
  (Right tl) <- parse parseTimeline "" <$> TIO.readFile fp

  let mp = minimum $ map (fromIntegral . toModifiedJulianDay . Mktl.Parser.start) tl
      a =
        map
          ( \e ->
              ( (\(x, y) -> (x, y + 1)) $ both ((`div` d) . subtract mp . fromIntegral . toModifiedJulianDay) (start e, end e)
              , desc e
              )
          )
          tl
      ys =
        map
          ( \e ->
              both ((\y -> (y, (y * 365 - mp - 678517) `div` d)) . year) (start e, end e)
          )
          tl

  print a
  print ys

  render a $ concatMap biList ys

-- mapM_ (\((s, e), dc) -> putStrLn $ concat [replicate s ' ', replicate (e - s + 1) '#', "\n", replicate s ' ', unpack dc, "\n"]) a
