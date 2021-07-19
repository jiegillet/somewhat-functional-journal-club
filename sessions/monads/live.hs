import Numeric.Probability.Example.Dice
import 
x = do
  n <- Just 2
  if n == 1 
    then return "Yay"
    else return "Boo"

f x = do
   y <- 1
   z <- y `div` x
   return ( z*z)

safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

g :: Int -> Maybe Int
g x = do
  --let y = 100
  y <- return 1
  z <- safeDiv y x
  a <- safeDiv z x
  return (z*z + a)

g' x = Just 1 >>= (\y ->
       safeDiv y x >>= (\z ->
       safeDiv z x >>= (\a ->
       return (z*z + a) )))

t :: Monad m => m Int
t = do
  i <- return 3
  y <- return $ i * 10
  return y
 
monadicAdd f g = do
  a <- f
  b <- if mod a 2 == 0 then g else return 0
  return (a + b)
