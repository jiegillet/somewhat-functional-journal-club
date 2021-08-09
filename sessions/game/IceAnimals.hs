import Data.Ratio ((%), Ratio)
import qualified Numeric.Probability.Distribution as Dist
import Prelude hiding (round)

-- Probabilities

type Probability = Rational

type Dist = Dist.T Probability

data Die = Ice | Igloo | Bridge

die :: Dist Die
die = Dist.uniform [Ice, Igloo, Bridge]

-- Game state

data GameState
  = GameState {iceberg :: Int, bridge :: Int, igloo :: Int, icePillars :: Int}
  | Win {icePillars :: Int}
  | Lose {iceberg :: Int, bridge :: Int, igloo :: Int}
  deriving (Eq, Ord, Show)

win :: GameState -> Bool
win (Win _) = True
win _ = False

lose :: GameState -> Bool
lose (Lose _ _ _) = True
lose _ = False

winOrLose :: GameState -> Bool
winOrLose s = win s || lose s

start :: GameState
start = GameState 4 0 0 6

-- Gameplay

play :: Die -> GameState -> GameState
play _ state@(Win _) = state
play _ state@(Lose _ _ _) = state
play roll state@(GameState iceberg bridge igloo icePillars) =
  case roll of
    Igloo -> case (iceberg, bridge) of
      (0, 1) -> Win icePillars
      (_, 0) -> state
      _ -> GameState iceberg (bridge - 1) (igloo + 1) icePillars
    Ice ->
      if icePillars == 1
        then Lose iceberg bridge igloo
        else GameState iceberg bridge igloo (icePillars - 1)
    Bridge ->
      if iceberg > 0
        then GameState (iceberg - 1) (bridge + 1) igloo icePillars
        else state

round :: GameState -> Dist GameState
round state = do
  roll <- die
  return $ play roll state

rounds :: Int -> Dist GameState
rounds 0 = return start
rounds n = Dist.norm $ (rounds (n -1)) >>= round

-- Analysis

roundsToFinish :: Int
roundsToFinish = length $ takeWhile unlikelyFinished $ map rounds [0 ..]
  where
    unlikelyFinished state = (winOrLose Dist.?? state) < 95 % 100

likelyEnd :: IO ()
likelyEnd = Dist.filter winOrLose (rounds roundsToFinish) Dist.//% ()

likelyTo :: (GameState -> Bool) -> Float
likelyTo end = fromRational $ Dist.truth (end <$> rounds roundsToFinish)
