import Data.Ord (Down(..), comparing)
import Data.List (group, sort, sortOn, elemIndex, sortBy, find, findIndex)
import Data.Maybe
import Data.Function (on)
import Text.Printf (printf)

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
  deriving ( Eq, Show, Ord )

type CardSet = String

type CardGrouper = CardSet -> [CardSet]

groupCards1 :: CardGrouper
groupCards1 = sortOn (Down . length) . group . sort

-- Ugly, I know. But it appends all jokers to the longtest sub-group of cards
groupCards2 :: CardGrouper
groupCards2 "JJJJJ" = ["JJJJJ"]
groupCards2 cards = zipWith appendJokersIfLongest [0..] withoutJokers
  where
    grouped = groupCards1 cards
    jokers = fromMaybe [] $ find ((== 'J') . head) grouped
    withoutJokers = filter ((/= 'J') . head) grouped
    longestGroupLength = maximum $ map length grouped
    longestGroupIndex = fromJust $ findIndex ((== longestGroupLength) . length) grouped
    appendJokersIfLongest ix group
      | ix == longestGroupIndex = group <> jokers
      | otherwise = group


handType :: CardGrouper -> CardSet -> HandType
handType groupCards cards = case groupCards cards of
  [[_,_,_,_,_]]   -> FiveOfAKind
  [[_,_,_,_],_]   -> FourOfAKind
  [[_,_,_],[_,_]] -> FullHouse
  [[_,_,_],_,_]   -> ThreeOfAKind
  [[_,_],[_,_],_] -> TwoPair
  [[_,_],_,_,_]   -> OnePair
  [_,_,_,_,_]     -> HighCard
  _               -> error $ "Bad hand: " <> cards

data Hand = MkHand { hType :: !HandType, cards :: !CardSet }
  deriving ( Eq, Show )

type CardValue = Char -> Int

cardValue1 :: Char -> Int
cardValue1 c = len - fromJust (elemIndex c allCards)
  where
    len = length allCards
    allCards = "AKQJT98765432"

cardValue2 :: Char -> Int
cardValue2 c = len - fromJust (elemIndex c allCards)
  where
    len = length allCards
    allCards = "AKQT98765432J"

cmpCardSets :: CardValue -> CardSet -> CardSet -> Ordering
cmpCardSets _ [] _ = EQ
cmpCardSets _ _ [] = EQ
cmpCardSets cardValue (x:xs) (y:ys) = case comparing cardValue x y of
  EQ       -> cmpCardSets cardValue xs ys
  ordering -> ordering

cmpHands :: CardValue -> Hand -> Hand -> Ordering
cmpHands cardValue (MkHand type1 cards1) (MkHand type2 cards2) = case compare type1 type2 of
  EQ       -> cmpCardSets cardValue cards1 cards2
  ordering -> ordering

type Bid = Int

rankings :: CardGrouper -> CardValue -> [(CardSet, Bid)] -> [(Int, Hand, Bid)]
rankings groupCards cardValue = zipWith mkTriple [1..] . sortBy (cmpHands cardValue `on` fst) . map mkHand
  where
    mkHand (cardSet, bid) = (MkHand (handType groupCards cardSet) cardSet, bid)
    mkTriple a (b,c) = (a,b,c)

parseInput :: String -> [(CardSet, Bid)]
parseInput = map parseLine . lines
  where
    parseLine = (\[cards, bid] -> (cards, read bid)) . words

type Ranker = [(CardSet, Bid)] -> [(Int, Hand, Bid)]

rankings1 :: Ranker
rankings1 = rankings groupCards1 cardValue1

rankings2 :: Ranker
rankings2 = rankings groupCards2 cardValue2

solve :: Ranker -> String -> Int
solve ranking = sum . map score . ranking . parseInput
  where
    score (ranking, _ , bid) = ranking * bid

main = do
  input <- readFile "input.txt"
  printf "Star 1: %d\n" $ solve rankings1 input
  printf "Star 2: %d\n" $ solve rankings2 input

