import System.IO
import Data.Char
import Data.List
import Data.Tuple

readInt :: String -> Int
readInt str = read str :: Int

readRaster :: String -> [[Int]]
readRaster str = do
  let strLines = lines str
  let strWords = map words strLines
  map (map readInt) $ strWords

range :: [Int] -> Int
range col = maximum col - minimum col

solve1 :: [[Int]] -> Int
solve1 raster = sum $ map range raster

tuplify2 [x,y] = (maximum [x,y], minimum[x,y])

combinations ns = map tuplify2 $ filter ((2==).length) $ subsequences ns

divisor col = do
  let candidates = combinations col
  let divModPairs = map (uncurry divMod) candidates
  let emptyRest = [value | (value, rest) <- divModPairs, rest == 0]
  emptyRest !! 0

solve2 :: [[Int]] -> Int
solve2 raster = sum $ map divisor raster

main = do
  content <- readFile "input.txt"
  let raster = readRaster content
  print $ solve1 raster
  print $ solve2 raster

