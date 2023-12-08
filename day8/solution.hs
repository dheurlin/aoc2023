import qualified Data.Map as Map
import qualified Control.Monad.Par as Par
import Control.Monad
import Data.Maybe ( fromJust )
import Text.Printf (printf)

type Node = String
type Instruction = Char
data Connections = MkConnections { left :: !Node, right :: !Node }
  deriving ( Eq, Show )

type Network = Map.Map Node Connections

lookup' :: (Ord k, Show k) => k -> Map.Map k a -> a
lookup' k map = case Map.lookup k map of
  Just a -> a
  Nothing -> error $ "Bad key: " <> show k


follow1 :: [Instruction] -> Node -> Network -> Int
follow1  _ "ZZZ" _ = 0
follow1 ('L':rest) currentKey network = 1 + follow1 rest (left $ lookup' currentKey network) network
follow1 ('R':rest) currentKey network = 1 + follow1 rest (right $ lookup' currentKey network) network
follow1 _ _ _ = error "lmao"

parseConnection :: String -> (Node, Connections)
parseConnection s = (node, connection)
  where
    node = head $ words s
    parens = tail $ dropWhile (/= '(') s
    left = takeWhile (/= ',') parens
    right = takeWhile (/= ')') $ tail $ dropWhile (/= ' ') parens
    connection = MkConnections left right

star1 :: String -> Int
star1 input = follow1 instructions firstNode network
  where
    (instructions, network) = parseInput input
    firstNode = head $ Map.keys network

parseInput :: String -> ([Instruction], Network)
parseInput input = (cycle firstLine, Map.fromList connections)
  where
    firstLine = head $ lines input
    restOfLines = tail $ tail $ lines input
    connections = map parseConnection restOfLines

direction :: Instruction -> Connections -> Node
direction 'L' = left
direction 'R' = right
direction i = error $ "Bad instruction: " <> show i

follow2 :: Par.IVar Bool -> [Par.IVar Bool] -> [Instruction] -> Node -> Network -> Par.Par Int
follow2 myFlag flags (i:is) curr@[_,_,'Z'] network = do
  Par.put myFlag True
  othersDone <- and <$> mapM Par.get flags
  if othersDone
    then pure 0
    else do
      res <- follow2 myFlag flags is (direction i $ lookup' curr network) network
      pure $ 1 + res
follow2 myFlag flags (i:is) curr network = do
  res <- follow2 myFlag flags is (direction i $ lookup' curr network) network
  pure $ 1 + res
follow2 _ _ _ _ _ = error "fuck me"

runFollow2 :: [Instruction] -> Network -> Int
runFollow2 instructions network = Par.runPar $ do
  let startKeys = filter ((== 'A') . last) $ Map.keys network
  flags <- replicateM (length startKeys) (Par.new :: Par.Par (Par.IVar Bool))
  res <- zipWithM (\key flag -> follow2 flag flags instructions key network) startKeys flags
  pure $ head res

star2 :: String -> Int
star2 input = runFollow2 instructions network
  where
    (instructions, network) = parseInput input

main = do
  input <- readFile "test-input-2.txt"
  -- printf "Star 1: %d\n" $ star1 input
  printf "Star 2: %d\n" $ star2 input
