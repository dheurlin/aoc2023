import Text.Printf ( printf )
import Data.Maybe ( mapMaybe )

-- NOTE: Star 2 is pretty slow, so you need to compile with optimizations for it to run in
-- reasonable time: `ghc -O2 -o solution solution.hs`

data Card = MkCard { winning :: ![Int], mine :: ![Int] }
  deriving ( Eq, Show )

parseCard :: String -> Card
parseCard str = MkCard { winning = winning, mine = mine }
  where
    withoutHeader = drop 2 $ dropWhile (/= ':') str
    winning = map read $ words $ takeWhile (/= '|') withoutHeader
    mine = map read $ words $ drop 2 $ dropWhile (/= '|') withoutHeader

parseCards :: String -> [Card]
parseCards = map parseCard . lines

matchingNumbers :: Card -> Int
matchingNumbers (MkCard winning mine) = length $ filter (`elem` mine) winning

score :: Card -> Int
score card
  | numMatching == 0 = 0
  | otherwise = 2 ^ (numMatching - 1)
  where
    numMatching = matchingNumbers card

star1 :: String -> Int
star1 = sum . map score . parseCards

lookupEntry :: Eq k => k -> [(k, v)] -> Maybe (k, v)
lookupEntry _ [] = Nothing
lookupEntry needle (entry:entries)
  | fst entry == needle = Just entry
  | otherwise           = lookupEntry needle entries

type NumberedCard = (Int, Card)

makeCopiesFromScore :: [NumberedCard] -> NumberedCard -> [NumberedCard]
makeCopiesFromScore originalCards numCard@(index, card) = mapMaybe (`lookupEntry` originalCards) copyIndices
  where
    theScore = matchingNumbers card
    copyIndices = map (+ (index + 1)) [0..theScore - 1]

play :: [NumberedCard] -> [NumberedCard] -> [NumberedCard]
play originalCards [] = []
play originalCards (card:deck) = card : play originalCards deckWithCopies
  where
    copiesThisRound = makeCopiesFromScore originalCards card
    deckWithCopies = copiesThisRound <> deck

star2 :: String -> Int
star2 input = length $ play cards cards
  where cards = zip [1..] $ parseCards input

-- Made a point-free version too just for fun. We can use the so-called "warbler combinator" to express this

warbler :: (a -> a -> b) -> a -> b
warbler f a = f a a

star2PointFree :: String -> Int
star2PointFree = length . warbler play . zip [1..] . parseCards

main = do
  input <- readFile "input.txt"
  printf "Star 1: %d\n" $ star1 input
  printf "Star 2: %d\n" $ star2PointFree input
