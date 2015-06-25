module Main where

import Graphics.GD
import Data.Tuple
import Data.Set (Set)
import qualified Data.Set as Set

black = rgb 0 0 0
white = rgb 255 255 255

applyEach :: [a -> b] -> [a] -> [b]
applyEach fs xs = map (uncurry ($)) (zip fs xs)

cellDrawer :: Int -> Point -> Bool -> (Image -> IO Image)
cellDrawer cellSize pos@(x, y) on =
  tap (drawFilledRectangle pos (cellSize + x, cellSize + y) color)
  where color = if on then black else white

rowDrawers :: Point -> Int -> [Bool] -> [Image -> IO Image]
rowDrawers (x, y) cellSize =
  let xs = map ((x +) . (cellSize *)) [0..]
      drawer = cellDrawer cellSize . (flip (,) $ y)
  in applyEach (map drawer xs)

drawRow :: Int -> Int -> [Bool] -> Image -> IO Image
drawRow cellSize y flags img =
  foldl (>>=) (return img) (rowDrawers (0, y) cellSize flags)

drawRows cellSize y rows img =
  let ys = map (cellSize *) [0..]
  in foldl (>>=) (return img) (map (uncurry $ drawRow cellSize) (zip ys rows))

segments3 :: [Int] -> [[Int]]
segments3 xs =
  let space = [0] ++ xs ++ [0]
  in map ((\n -> map (((!!) space) . (+ n)) [-1, 0, 1]) . fst) (zip [1..] xs)

next :: ([Int] -> Bool) -> [Int] -> [Int]
next rule xs =
  map (\ys -> if rule ys then 1 else 0) (segments3 xs)

rootRow n =
  let (fr, b) = splitAt (n `quot` 2) (replicate n 0) 
  in fr ++ [1] ++ b

rows :: ([Int] -> Bool) -> [Int] -> Int -> [[Int]]
rows rule row1 1 = [row1]
rows rule row1 n = row1:(map (next rule) (row1:(rows rule row1 (n-1))))

toBin 0 = [0]
toBin n | n `mod` 2 == 1 = toBin (n `div` 2) ++ [1]
        | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]

lpad m xs = reverse . (take m) . reverse $ (take m $ repeat 0) ++ (take m xs)
toBin' n = lpad 8 $ toBin n

opts = reverse $ map ((reverse . (take 3) . (++ (repeat 0)) . reverse) . toBin) [0..7]
rule n xs = Set.member xs validConfigs
  where validConfigs = Set.fromList (map fst (filter ((==1) . snd) (zip opts (toBin' n))))

tap :: Monad m => (a -> m b) -> a -> m a
tap fn c = do fn c; return c

main :: IO ()
main =
  let w = 200; h = 200; s = 5
      n = h `quot` s in
  do newImage (w, h)
     >>= tap (fillImage white)
     >>= tap (drawRows s 0 (map (map (1==)) (rows (rule 110) (rootRow n) n)))
     >>= savePngFile "grid.png"

