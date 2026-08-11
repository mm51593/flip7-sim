import System.Random (newStdGen, StdGen, split)
import System.Random.Shuffle (shuffle')
import Data.List (delete, unfoldr)
import Control.Monad.State
import Control.Monad (replicateM)

data Rank =
    Zero | One | Two | Three | Four |
    Five | Six | Seven | Eight | Nine |
    Ten | Eleven | Twelve
    deriving (Show, Enum, Bounded, Eq)

type Deck = [Rank]

data Game = Game Deck StdGen

makeCopies :: Rank -> [Rank]
makeCopies Zero = [Zero]
makeCopies rank = replicate (fromEnum rank) rank

constructDeck :: Deck
constructDeck = concat [
        makeCopies rank
        | rank <- [minBound .. maxBound]
    ]

shuffle :: StdGen -> [a] -> ([a], StdGen)
shuffle rng xs = (newList, nextGen)
    where
        (curGen, nextGen) = split rng
        newList = shuffle' xs (length xs) curGen

reshuffleDeck :: StdGen -> [Rank] -> Game
reshuffleDeck rng hand = Game shuffledDeck nextRng
    where
        newDeck = foldr delete constructDeck hand
        (shuffledDeck, nextRng) = shuffle rng newDeck

drawUntilBust :: Game -> (Game, [Rank])
drawUntilBust g = go g []
    where
        go :: Game -> [Rank] -> (Game, [Rank])
        go (Game [] rng) hand = go (reshuffleDeck rng hand) hand
        go (Game (drawn:rest) rng) hand
          | drawn `elem` hand = (Game rest rng, hand)
          | otherwise         = go (Game rest rng) (drawn:hand)

sumHand :: [Rank] -> Int
sumHand hand = sum $ fmap fromEnum hand

playRound :: State Game Int
playRound = do
    game <- get
    let (newGame, hand) = drawUntilBust game
    put newGame
    return (sumHand hand)

playRounds :: Game -> [Int]
playRounds =
    unfoldr $ \game -> Just (runState playRound game)

avg :: [Int] -> Double
avg pts = fromIntegral (sum pts) / fromIntegral (length pts)

main :: IO ()
main = do
    rng <- newStdGen
    let initialGame = Game [] rng
        pts = take 2000 (playRounds initialGame)
    print $ avg pts
