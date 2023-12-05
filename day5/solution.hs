import Data.Foldable ( asum )
import Data.Maybe (fromMaybe)
import Data.List.Split
import Text.Printf ( printf )

data MappingEntry = MkEntry {sourceStart :: !Integer, destStart :: !Integer, len :: !Integer}
  deriving (Eq, Show)

type Mapping = [MappingEntry]

lookupInEntry :: Integer -> MappingEntry -> Maybe Integer
lookupInEntry sourceKey (MkEntry sourceStart destStart len)
  | sourceKey < sourceStart = Nothing
  | sourceKey > sourceStart + len - 1 = Nothing
  | otherwise = Just $ destStart + (sourceKey - sourceStart)

lookupInEntries :: Integer -> Mapping -> Maybe Integer
lookupInEntries key = asum . map (lookupInEntry key)

lookupInMapping :: Integer -> Mapping -> Integer
lookupInMapping key mapping = fromMaybe key (lookupInEntries key mapping)

followMappings :: Integer -> [Mapping] -> Integer
followMappings key [] = key
followMappings key (currMapping:rest) = followMappings result rest
  where result = lookupInMapping key currMapping

parseEntry :: String -> MappingEntry
parseEntry str = MkEntry { sourceStart = read src, destStart = read dest, len = read len }
  where [dest, src, len] = words str

parseMapping :: [String] -> Mapping
parseMapping = map parseEntry . tail

paragraphs :: String -> [[String]]
paragraphs str = splitOn [""] $ lines str

parseInput :: String -> ([Integer], [Mapping])
parseInput input = (seeds, mappings)
  where
    input' = paragraphs input
    seeds = map read $ words $ dropWhile (/= ' ') $ (head . head) input'
    mappings = map parseMapping $ tail input'

star1 :: String -> Integer
star1 input = minimum $ map (`followMappings` mappings) seeds
  where (seeds, mappings) = parseInput input
  
main = do
  input <- readFile "input.txt"
  printf "Star 1: %d\n" $ star1 input

