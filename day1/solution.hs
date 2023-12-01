import Data.Char (isDigit)
import Data.List (find, isPrefixOf)
import Data.Maybe (fromJust)
import Text.Printf (printf)

firstAndLast :: [a] -> [a]
firstAndLast xs = [head xs, last xs]

star1 :: String -> Int
star1 = sum . map solveLine . lines
  where
    solveLine :: String -> Int
    solveLine = read . firstAndLast . filter isDigit

wordDigits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

validDigits = map show [1 .. 9] <> wordDigits

translateToDigit :: String -> String
translateToDigit str = fromJust $ lookup str digitMapping
  where
    digitMapping = zip validDigits (cycle $ map show [1 .. 9])

findValidDigits :: String -> [String]
findValidDigits "" = []
findValidDigits str =
  let maybePrefix = find (`isPrefixOf` str) validDigits
   in case maybePrefix of
        (Just prefix) -> prefix : findValidDigits (drop (length prefix) str)
        _ -> findValidDigits $ tail str

star2 :: String -> Int
star2 = sum . map solveLine . lines
  where
    solveLine :: String -> Int
    solveLine = read . concat . firstAndLast . map translateToDigit . findValidDigits

main = do
  input <- readFile "input.txt"
  printf "Star 1: %d\n" $ star1 input
  printf "Star 2: %d\n" $ star2 input
