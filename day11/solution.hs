import Data.List
import Data.Function
import Data.Maybe
import Text.Printf

type GalaxyMap = [[Char]]
type Coords = (Int, Int)

isGalaxy :: Char -> Bool
isGalaxy '#' = True
isGalaxy _ = False

isEmpty :: Char -> Bool
isEmpty '.' = True
isEmpty _ = False

expandNaive :: Int -> GalaxyMap -> GalaxyMap
expandNaive n = transpose . expand' . transpose . expand'
  where
    expand' = concatMap expandRow
    expandRow row
      | all isEmpty row = replicate n row
      | otherwise         = [row]

galaxies :: GalaxyMap -> [Coords]
galaxies gmap = concat $ zipWith solveLine [0..] gmap
  where
    solveLine :: Int -> [Char] -> [Coords]
    solveLine y line = catMaybes $ zipWith (maybeCoords y) [0..] line
    maybeCoords y x point
      | isGalaxy point = Just (x, y)
      | otherwise       = Nothing

data GalaxyMapEntry = Empty | Expansion | Galaxy { getGalaxy :: !Coords }
  deriving ( Eq, Show )

galaxiesAndExpansions :: GalaxyMap -> [[GalaxyMapEntry]]
galaxiesAndExpansions = insertColumnExpansions . zipWith solveLine [0..]
  where
    solveLine y line
      | all isEmpty line = replicate (length line) Expansion
      | otherwise = zipWith (mkEntry y) [0..] line
    mkEntry y x point
      | isGalaxy point = Galaxy (x, y)
      | isEmpty point  = Empty
      | otherwise = error $ "invalid character in input: " <> show point
    insertColumnExpansions = transpose . map substitute . transpose
    substitute col
      | all (\point -> point == Empty || point == Expansion) col = replicate (length col) Expansion
      | otherwise = col

expandSmarter :: Int -> [[GalaxyMapEntry]] -> [[GalaxyMapEntry]]
expandSmarter n = transpose . map (expand updateY 0) . transpose . map (expand updateX 0)
  where
    expand _ _ [] = []
    expand update currentExpansion (entry:entries) = case entry of
      Empty -> Empty : expand update currentExpansion entries
      Expansion -> Expansion : expand update (currentExpansion + 1) entries
      Galaxy coords -> Galaxy (update currentExpansion coords) : expand update currentExpansion entries

    updateY expansion (x, y) = (x, y + (n * expansion))
    updateX expansion (x, y) = (x + (n * expansion), y)

galaxyMapCoords :: [[GalaxyMapEntry]] -> [Coords]
galaxyMapCoords = concatMap coordsInLine
  where
    coordsInLine = mapMaybe maybeCoords
    maybeCoords (Galaxy (x,y)) = Just (x,y)
    maybeCoords _ = Nothing

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

-- testInput = lines <$> readFile "test-input.txt"

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x1, y1) (x2, y2) = ((+) `on` abs) (x1 - x2) (y1 - y2)

star1Naive :: String -> Int
star1Naive input = sum $ map (uncurry manhattan) galaxyPairs
  where
    gmap = lines input
    expanded = expandNaive 2 gmap
    galaxyPairs = pairs $ galaxies expanded

star1Smarter :: String -> Int
star1Smarter input = sum $ map (uncurry manhattan) galaxyPairs
  where
    gmap = lines input
    expanded = expandSmarter (2 - 1) $ galaxiesAndExpansions gmap
    galaxyPairs = pairs $ galaxyMapCoords expanded

-- Doesn't halt, even for test input
star2Naive :: String -> Int
star2Naive input = sum $ map (uncurry manhattan) galaxyPairs
  where
    gmap = lines input
    expanded = expandNaive 1000000 gmap
    galaxyPairs = pairs $ galaxies expanded

star2Smarter :: String -> Int
star2Smarter input = sum $ map (uncurry manhattan) galaxyPairs
  where
    gmap = lines input
    expanded = expandSmarter (1000000 - 1) $ galaxiesAndExpansions gmap
    galaxyPairs = pairs $ galaxyMapCoords expanded

main = do
  input <- readFile "input.txt"
  printf "Star 1: %d\n" $ star1Smarter input
  printf "Star 2: %d\n" $ star2Smarter input


