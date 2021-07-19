module State where

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap f (State st) = State $ \s -> 
     let (a, s') = st s 
     in (f a, s')

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  (State f) <*> (State st) = State $ \s ->
    let (fa, s') = f s
        (a, s'') = st s'
     in (fa a, s'')

instance Monad (State s) where
  return = pure
  (State st) >>= f = State $ \s ->
    let (a, s') = st s
        (State sf) = f a
     in sf s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ const ((), s)

state :: (s -> (a, s)) -> State s a
state = State

-- runState :: State a s -> s -> (a, s)

evalState :: State s a -> s -> a
evalState (State st) = fst . st

execState :: State s a -> s -> s
execState (State st) = snd . st
