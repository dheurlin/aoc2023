import Data.Function
import Data.List
import Data.List.Split
import Text.Printf

data Spring = Broken | Operational | Unknown
  deriving ( Eq, Show )

parseSpring :: Char -> Spring
parseSpring '.' = Operational
parseSpring '#' = Broken
parseSpring '?' = Unknown
parseSpring c = error $ "Unknown character in input: " <> show c

allSubstitutions :: [Spring] -> [[Spring]]
allSubstitutions [] = [[]]
allSubstitutions (Unknown:rest) = withBroken <> withOperational
  where
    withBroken      = map (Broken :) $ allSubstitutions rest
    withOperational = map (Operational :) $ allSubstitutions rest
allSubstitutions (x:xs) = map (x :) $ allSubstitutions xs
  
satisfiesGroups :: [Spring] -> [Int] -> Bool
satisfiesGroups springs expectedLengths = actualLengths == expectedLengths
  where
    actualLengths = map length $ filter ((== Broken) . head) $ group springs

possibleInLine :: [Spring] -> [Int] -> Int
possibleInLine springs groups = length [ spring | spring <- allSubstitutions springs, satisfiesGroups spring groups ]

parseInput :: String -> [([Spring], [Int])]
parseInput = map parseLine . lines
  where
    parseLine line
      | [springInput, groupInput] <- words line = (parseSprings springInput, parseGroups groupInput)
      | otherwise = error "sdf"

    parseSprings = map parseSpring
    parseGroups = map read . splitOn ","

star1 :: String -> Int
star1 = sum . map (uncurry possibleInLine) . parseInput

main = do
  input <- readFile "input.txt"
  printf "Star 1: %d\n" $ star1 input

