import Data.List (transpose)
import Text.Printf

type Time = Int
type Distance = Int

distanceTravelled :: Time -> Time -> Distance
distanceTravelled totalTime heldTime = travelTime * speed
  where
    travelTime = totalTime - heldTime
    speed = heldTime

possibleDistances :: Time -> [Distance]
possibleDistances totalTime = map (distanceTravelled totalTime) [0..totalTime]

waysToBeat :: Time -> Distance -> Int
waysToBeat totalTime record = length . filter (> record) . possibleDistances $ totalTime

type Race = (Time, Distance)

parseInput1 :: String -> [Race]
parseInput1 = map makeTuple . transpose .  map extractNumbers . lines
  where
    dropPrefix = tail . dropWhile (/= ':')
    extractNumbers = map read . words . dropPrefix

makeTuple [a, b] = (a, b)
makeTuple _ = error "Invalid input"

parseInput2 :: String -> Race
parseInput2 = makeTuple . map (read . filter (/= ' ') . dropPrefix) . lines
  where
    dropPrefix = tail . dropWhile (/= ':')

star1 :: String -> Int
star1 = product . map (uncurry waysToBeat) . parseInput1

star2 :: String -> Int
star2 = uncurry waysToBeat . parseInput2

main = do
  input <- readFile "input.txt"
  printf "Star 1: %d\n" $ star1 input
  printf "Star 2: %d\n" $ star2 input
