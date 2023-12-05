import Data.Foldable ( asum )
import Data.Maybe (fromMaybe)
import Data.List.Split
import Text.Printf ( printf )
import Data.List
import Data.Function (on)

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

type Range = (Integer, Integer)

rangeOverlap :: Range -> MappingEntry -> Range
rangeOverlap (rangeStart, rangeLength) (MkEntry srcStart destStart entryLen) = (overlapStart, overlapLen)
  where
    rangeEnd = rangeStart + rangeLength - 1
    entryEnd = srcStart + entryLen - 1
    overlapMin = max rangeStart srcStart
    overlapMax = min rangeEnd entryEnd
    overlapLen = max 0 $ 1 + overlapMax - overlapMin
    overlapStart = destStart + (overlapMin - srcStart)


rangesInEntry :: Range -> MappingEntry -> [Range]
rangesInEntry r@(rangeStart, rangeLength) e@(MkEntry srcStart destStart entryLen) =
  -- [before, overlap, after]
  filter ((/= 0) . snd) [before, overlap, after]
    where
      before@(_,beforeLen) = (rangeStart, max 0 (srcStart - rangeStart))
      overlap@(_, overlapLen) = rangeOverlap r e
      after = (rangeStart + beforeLen + overlapLen, max 0 (rangeLength - overlapLen - beforeLen))

rangesInMapping :: Range -> Mapping -> [Range]
rangesInMapping r = concatMap $ rangesInEntry r

followRangeMappings :: Range -> [Mapping] -> [Range]
followRangeMappings r [] = [r]
followRangeMappings r (mapping:rest) = concatMap (`followRangeMappings` rest) result
  where
    result = rangesInMapping r mapping

minimumRanges :: [Range] -> Integer
minimumRanges = minimum . map fst

parseSeedRanges :: String -> [Range]
parseSeedRanges input = go $ words $ dropWhile (/= ' ') input
  where
    go [] = []
    go (a:b:xs) = (read a, read b) : go xs
    go _        = error "Bad input"

parseInput2 :: String -> ([Range], [Mapping])
parseInput2 input = (seedRanges, mappings)
  where
    input' = paragraphs input
    seedRanges = parseSeedRanges $ (head . head) input'
    mappings = map parseMapping $ tail input'

-- star2 :: String -> Integer
-- star2 input = minimum $ evalRanges $ concatMap (`followRangeMappings` mappings) seedRanges
-- star2 input = minimumRanges $ concatMap (`followRangeMappings` mappings) seedRanges
star2 input = take 1000 $ sortBy (compare `on` fst) $ concatMap (`followRangeMappings` mappings) seedRanges
  where (seedRanges, mappings) = parseInput2 input

-- main = do
--   input <- readFile "test-input.txt"
--   -- printf "Star 1: %d\n" $ star1 input
--   printf "Star 2: %d\n" $ star2 input

