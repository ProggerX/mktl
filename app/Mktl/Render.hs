module Mktl.Render where

import Data.Text (Text, unpack)
import Debug.Trace
import Diagrams.Backend.SVG
import Diagrams.Prelude

drawArc :: ((Int, Int), Text) -> Diagram B
drawArc ((s, e), t) = beside (r2 (0, 1)) (arcBetween (p2 (-l, 0)) (p2 (l, 0)) (l / 5)) (scale 5 $ moveOriginBy (r2 (0, 10)) $ text (unpack t) <> rect 1 1 # lineColor transparent)
 where
  l = fromIntegral (e - s) / 2

drawYear :: (Int, Int) -> Diagram B
drawYear (y, _) = scale 10 $ moveOriginBy (r2 (0, -4.5)) $ vrule 10 # dashingG [10, 5] 0 # lc gray # lw 0.5 <> text (show y) <> rect 1 1 # lineColor transparent

d :: [((Int, Int), Text)] -> Int -> [(Int, Int)] -> Diagram B
d a mw ys =
  moveTo (p2 (-(fromIntegral mw / 2), 0)) (atPoints (map (p2 . (,0) . (\((s, e), _) -> fromIntegral $ (e - s) `div` 2 + s)) a) (map drawArc a))
    <> moveTo (p2 (-(fromIntegral mw / 2), 0)) (atPoints (map (p2 . (,0) . fromIntegral . snd) ys) (map drawYear ys))
    <> hrule (fromIntegral mw * 1.5)
    <> moveOriginBy (r2 (0, -50)) (rect (fromIntegral mw * 1.5) 200 # fillColor white)

render :: [((Int, Int), Text)] -> [(Int, Int)] -> IO ()
render a ys = do
  let mw = maximum $ map (snd . fst) a
  print mw
  renderSVG "out.svg" (mkSizeSpec2D Nothing Nothing) (d a mw ys)
  pure ()
