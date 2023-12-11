import Data.List
import Data.Function
import Data.Maybe
import Text.Printf

type Coords = (Int, Int)

isGalaxy :: Char -> Bool
isGalaxy '#' = True
isGalaxy _ = False

isEmpty :: Char -> Bool
isEmpty '.' = True
isEmpty _ = False

data GalaxyMapEntry = Empty | Expansion | Galaxy { getGalaxy :: !Coords }
  deriving ( Eq, Show )

type GalaxyMap = [[GalaxyMapEntry]]

makeGalaxyMap :: [[Char]] -> GalaxyMap
makeGalaxyMap = insertColumnExpansions . zipWith solveLine [0..]
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

expandMap :: Int -> GalaxyMap -> GalaxyMap
expandMap n = transpose . map (expand updateY 0) . transpose . map (expand updateX 0)
  where
    expand _ _ [] = []
    expand update currentExpansion (entry:entries) = case entry of
      Empty -> Empty : expand update currentExpansion entries
      Expansion -> Expansion : expand update (currentExpansion + 1) entries
      Galaxy coords -> Galaxy (update currentExpansion coords) : expand update currentExpansion entries

    updateY expansion (x, y) = (x, y + (n - 1) * expansion)
    updateX expansion (x, y) = (x + (n - 1) * expansion, y)

galaxyMapCoords :: GalaxyMap -> [Coords]
galaxyMapCoords = concatMap coordsInLine
  where
    coordsInLine = mapMaybe maybeCoords
    maybeCoords (Galaxy (x,y)) = Just (x,y)
    maybeCoords _ = Nothing

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x1, y1) (x2, y2) = ((+) `on` abs) (x1 - x2) (y1 - y2)

star1Smarter :: String -> Int
star1Smarter input = sum $ map (uncurry manhattan) galaxyPairs
  where
    gmap = lines input
    expanded = expandMap 2 $ makeGalaxyMap gmap
    galaxyPairs = pairs $ galaxyMapCoords expanded

star2Smarter :: String -> Int
star2Smarter input = sum $ map (uncurry manhattan) galaxyPairs
  where
    gmap = lines input
    expanded = expandMap 1000000 $ makeGalaxyMap gmap
    galaxyPairs = pairs $ galaxyMapCoords expanded

main = do
  input <- readFile "input.txt"
  printf "Star 1: %d\n" $ star1Smarter input
  printf "Star 2: %d\n" $ star2Smarter input


