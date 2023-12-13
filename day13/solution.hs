import Data.List
import Data.List.Split (splitOn)
import Text.Printf
import Data.Maybe
import Data.Function

isReflection :: ([Char], [Char]) -> Bool
isReflection (lhs, rhs) = all (== True) $ zipWith (==) (reverse lhs) rhs

findReflectionsInLine :: [Char] -> [Int]
findReflectionsInLine line = findIndices isReflection reflections
  where reflections = [splitAt i line | i <- [1..length line - 1]]

findCommonElem :: Eq a => [[a]] -> Maybe a
findCommonElem [] = Nothing
findCommonElem ([]:xss) = Nothing
findCommonElem (xs:xss) = find (\x -> all (\other -> x `elem` other) xss) xs

-- findCommonElem :: Eq a => [[a]] -> [a]

data Reflection = Horizontal Int | Vertical Int
  deriving (Eq, Show)

cmpReflections :: Reflection -> Reflection -> Ordering
cmpReflections (Horizontal _) (Vertical _) = LT
cmpReflections (Vertical _) (Horizontal _) = GT
cmpReflections (Horizontal n) (Horizontal m) = compare n m
cmpReflections (Vertical n) (Vertical m) = compare n m

reflectionValue :: Reflection -> Int
reflectionValue (Horizontal n) = n + 1
reflectionValue (Vertical n) = (n + 1) * 100

reflectionInPattern :: String -> Maybe Reflection
reflectionInPattern pattern
  | Just col <- findCommonElem $ map findReflectionsInLine (lines pattern) = Just $ Horizontal col
  | Just row <- findCommonElem $ map findReflectionsInLine (transpose $ lines pattern) = Just $ Vertical row
  | otherwise = Nothing

star1 :: String -> Int
star1 input = sum $ map getValue patterns
  where
    getValue pattern = fromJust $ reflectionValue <$> reflectionInPattern pattern
    patterns = splitOn "\n\n" input

flipCell :: Char -> Maybe Char
flipCell '.' = Just '#'
flipCell '#' = Just '.'
flipCell c = Nothing

flipNth :: String -> Int -> Maybe String
flipNth str n = do
  let (before, after) = splitAt n str
  flipped <- flipCell (head after)
  pure $ before <> (flipped : tail after)

variationsOfPattern :: String -> [String]
variationsOfPattern pattern = mapMaybe (flipNth pattern) [0..(length pattern - 1)]

-- reflectionsInVariations :: String -> 

findVariationReflection :: String -> Maybe Reflection
findVariationReflection pattern = minimalGroup
  where
    variations = variationsOfPattern pattern
    reflections = mapMaybe reflectionInPattern variations
    reflectionGroups = group $ sortBy cmpReflections reflections
    minimalGroup = case minimumBy (compare `on` length) reflectionGroups of
      [] -> Nothing
      xs -> Just $ head xs

star2 :: String -> Int
star2 input = sum $ map getValue patterns
  where
    getValue pattern = fromJust $ reflectionValue <$> findVariationReflection pattern
    patterns = splitOn "\n\n" input

dude = unlines [ "#.##..##."
               , "..#.##.#."
               , "##......#"
               , "##......#"
               , "..#.##.#."
               , "..##..##."
               , "#.#.##.#."]

main = do
  input <- readFile "test-input.txt"
  printf "Star 1: %d\n" $ star1 input
  printf "Star 2: %d\n" $ star2 input
