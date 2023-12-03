import Data.Char
import Data.List.Split
import Text.Printf

data Area = MkArea { xCoords :: ![Int], yCoords :: ![Int] }
  deriving ( Eq, Show )

data NumLocation = MkNumLocation { number :: !Int
                                 , area   :: !Area
                                 }
  deriving ( Eq, Show )

numLocationFromXs :: Int -> [(Int, Char)] -> NumLocation
numLocationFromXs y xs = MkNumLocation (read number) $ MkArea span [y]
  where
    (span, number) = unzip xs

findNumLocations :: String -> [NumLocation]
findNumLocations input = findPerLine $ zip [0..] $ lines input
  where
    findPerLine :: [(Int, String)] -> [NumLocation]
    findPerLine indexedLines = concatMap findInLine indexedLines

    findInLine :: (Int, String) -> [NumLocation]
    findInLine (y, line) = map (numLocationFromXs y)
                            $ filter (not . null)
                            $ splitWhen (not . isDigit . snd)
                            $ zip [0..] line

surroundingArea :: Area -> Area
surroundingArea (MkArea xs ys) = MkArea (expand xs) (expand ys)
  where
    expand :: [Int] -> [Int]
    expand as = [minimum as - 1] <> as <> [maximum as + 1]

charsInArea :: String -> Area -> [Char]
charsInArea input (MkArea xs ys) = map getCoord coords
  where
    coords = [(x, y) | x <- xs, y <- ys]

    theLines = lines input
    maxX = length (head theLines) - 1
    maxY = length theLines - 1

    getCoord :: (Int, Int) -> Char
    getCoord (x, y)
      | x < 0 || y < 0 = '.'
      | x > maxX || y > maxY = '.'
      | otherwise            = theLines !! y !! x

star1 :: String -> Int
star1 input = sum $ map number adjacentToSymbol
  where
    numLocations = findNumLocations input 
    isSymbol x = not (isDigit x || x == '.')

    adjacentToSymbol :: [NumLocation]
    adjacentToSymbol = filter isAdjacentToSymbol numLocations

    isAdjacentToSymbol :: NumLocation -> Bool
    isAdjacentToSymbol loc = any isSymbol
                              $ charsInArea input
                              $ surroundingArea (area loc)

main = do
  input <- readFile "input.txt"
  printf "Star 1: %d\n" (star1 input)
