{- import Control.Monad.State

{-

    Custom State Monad

-}

data State' s a = State' (s -> (a, s))

state' :: (s -> (a, s)) -> State' s a
state' = State'

runState' :: State' s a -> s -> (a, s)
runState' (State' act) = act

evalState' :: State' s a -> s -> a
evalState' (State' act) = fst . act

(~>>=) :: State' s a -> (a -> State' s b) -> State' s b
-- (~>>=) :: (s -> (s, a)) -> (a -> s -> (s, b)) -> s -> (s,b)
(State' act) ~>>= f = State' (\s -> let (a, s1) = act s
                                        State' act1 = f a
                                     in act1 s1)

return' :: a -> State' s a
return' x = State' (\s -> (x, s))

-}
{-

    Example: Pseudo-random numbers

-}

import Control.Monad (liftM2)
import State

type Random a = State Int a

bsdRand :: Int -> Int
bsdRand n = (n * 1103515245 + 12345) `mod` 2 ^ 31

sample :: (Int -> a) -> Random a
sample f =
  state
    ( \s ->
        let rand_int = bsdRand s
         in (f rand_int, rand_int)
    )

randChoice :: [a] -> Random a
randChoice xs = sample (\i -> xs !! (i `mod` length xs))

rand01 :: Random Float
rand01 = sample (\s -> fromIntegral s / (2 ^ 31))

randAdd :: Num a => Random a -> Random a -> Random a
randAdd = liftM2 (+)

uniformMixture :: [(Float, Float)] -> Random Float
uniformMixture gs = do
  (a, b) <- randChoice gs
  x <- rand01
  return (a + x * (b - a))

twoRands :: Random a -> Random (a, a)
twoRands rand = do
  a <- rand
  b <- rand
  return (a, b)

manyRands :: Random a -> Int -> Random [a]
manyRands _ 0 = return []
manyRands rand n = (:) <$> rand <*> manyRands rand (n -1)

monadicAdd f g = do
  a <- f
  b <- if mod a 2 == 0 then g else return 0
  return (a + b)
