import Text.Printf (printf)

differences :: [Int] -> [Int]
differences xs = zipWith (-) (tail xs) xs

allZeroes :: [Int] -> Bool
allZeroes = all (== 0)

differencesTillZeroes :: [Int] -> [[Int]]
differencesTillZeroes xs
  | allZeroes xs = [xs]
  | otherwise = xs : differencesTillZeroes (differences xs)

extrapolateForward :: [[Int]] -> [[Int]]
extrapolateForward xs = reverse $ go 0 $ reverse xs
  where
    go _ [] = []
    go n (xs:xss) = (xs <> [n + last xs]) : go (n + last xs) xss

extrapolateBackwards :: [[Int]] -> [[Int]]
extrapolateBackwards xs = reverse $ go 0 $ reverse xs
  where
    go _ [] = []
    go n (xs:xss) = (head xs - n : xs) : go (head xs - n) xss

star1 :: String -> Int
star1 = sum . map (last . head . solveLine) . parse
  where
    parse = map (map read . words) . lines
    solveLine = extrapolateForward . differencesTillZeroes

star2 :: String -> Int
star2 = sum . map (head . head . solveLine) . parse
  where
    parse = map (map read . words) . lines
    solveLine = extrapolateBackwards . differencesTillZeroes

main = do
  input <- readFile "input.txt"
  printf "Star 1: %d\n" $ star1 input
  printf "Star 2: %d\n" $ star2 input
