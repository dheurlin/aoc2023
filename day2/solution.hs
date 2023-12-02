import Data.Char
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Text.Printf (printf)

data CubeSet = MkCubeSet { blue :: !Int, red :: !Int, green :: !Int }
  deriving ( Eq, Show )

type Game = [CubeSet]

addCubeSet :: CubeSet -> CubeSet -> CubeSet
addCubeSet c1 c2 = MkCubeSet { red = red c1 + red c2
                           , blue = blue c1 + blue c2
                           , green = green c1 + green c2
                           }

sumCubeSet :: [CubeSet] -> CubeSet
sumCubeSet = foldr addCubeSet $ MkCubeSet 0 0 0

maxCubeSet :: CubeSet -> CubeSet -> CubeSet
maxCubeSet c1 c2 = MkCubeSet { red = max (red c1) (red c2)
                           , blue = max (blue c1) (blue c2)  
                           , green = max (green c1) (green c2)
                           }

maximumCubeSet :: [CubeSet] -> CubeSet
maximumCubeSet = foldr1 maxCubeSet

cubeSetProduct :: CubeSet -> Int
cubeSetProduct c = red c * blue c * green c

-- Parses a single number, i.e. "3 red"
parseSingleCubeSet :: String -> CubeSet
parseSingleCubeSet str = case color of
  "blue"   -> MkCubeSet { blue = n, red = 0, green = 0 }
  "red"    -> MkCubeSet { blue = 0, red = n, green = 0 }
  "green"  -> MkCubeSet { blue = 0, red = 0, green = n }
  _        -> error $ "Parse error: invalid color " <> show color
  where
    numString = takeWhile isDigit str
    n = read numString :: Int
    color = drop (length numString + 1) str


-- Parses one draw, i.e "1 blue, 3 red"
parseDraw :: String -> CubeSet
parseDraw str = sumCubeSet cubeSets
  where
    cubeSets = map parseSingleCubeSet splits
    splits = splitOn ", " str

parseGame :: String -> Game
parseGame str = map parseDraw . splitOn "; " $ withoutPrefix
  where
    withoutPrefix = splitOn ": " str !! 1

possibleGame :: CubeSet -> Game -> Bool
possibleGame requrement game = 
  red requrement >= red gameMax &&
  green requrement >= green gameMax &&
  blue requrement >= blue gameMax
  where
    gameMax = maximumCubeSet game

star1 :: String -> Int
star1 input = sum possibleIndices
  where
    testGame = possibleGame MkCubeSet { red = 12, green = 13, blue = 14 }
    possibleIndices = map fst $ filter (testGame . snd) $ zip [1..] games
    games = map parseGame $ lines input

star2 :: String -> Int
star2 input = sum minimumGameProducts
  where
    minimumGameProducts = map cubeSetProduct maxima
    maxima = map maximumCubeSet games
    games = map parseGame $ lines input

main = do
  input <- readFile "input.txt"
  printf "Star 1: %d\n" $ star1 input
  printf "Star 2: %d\n" $ star2 input

