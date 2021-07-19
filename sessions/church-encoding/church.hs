{-# LANGUAGE RankNTypes #-}

module Church where

import Prelude hiding (and, or, pred, succ)

type N = forall a. (a -> a) -> a -> a

zero :: N
zero = \f x -> x

one :: N
one = \f x -> f x

two :: N
two = \f x -> f (f x)

three :: N
three = \f x -> f (f (f x))

num :: Int -> N
num 0 = zero
num n = \f x -> f (num (n - 1) f x)

toInt :: N -> Int
toInt n = n (+ 1) 0

toUnits :: N -> [()]
toUnits n = n (() :) []

toLaughs :: N -> String
toLaughs n = n ("ha" ++) ""

succ :: N -> N
succ a = \f x -> f (a f x)

add :: N -> N -> N
add a b = \f x -> a f (b f x)

-- mult :: N -> N -> N
mult a b = \f x -> a (b f) x

-- pow :: N -> N -> N
pow b e = \f x -> e b f x

-- poe w b = \f x -> b ( b ..(b f)) x

pred :: N -> N
pred n = \f x -> n (\n' f' -> f' (n' f)) (\_ -> x) (\a -> a)

-- pred 0 = \f x -> zero (whatever) (\_ -> x) (\a -> a)
--        = \f x -> (\_ -> x) (\a -> a)
--        = \f x -> x
--        = zero

-- pred 1 = \f x -> one (\n' f' -> f' (n' f)) (\_ -> x) (\a -> a)
--        = \f x -> (\n' f' -> f' (n' f)) (\_ -> x) (\a -> a)
--        = \f x -> (\a -> a) ((\_ -> x) f)
--        = \f x -> (\a -> a) x
--        = \f x -> x
--        = zero

-- pred 2 = \f x -> two (\n' f' -> f' (n' f)) (\_ -> x) (\a -> a)
--        = \f x -> (\n'' f'' -> f'' (n'' f)) ((\n' f' -> f' (n' f)) (\_ -> x)) (\a -> a)
--        = \f x -> (\n'' f'' -> f'' (n'' f)) ((\f' -> f' ((\_ -> x) f))) (\a -> a)
--        = \f x -> (\n'' f'' -> f'' (n'' f)) (\f' -> f' x) (\a -> a)
--        = \f x -> (\f'' -> f'' ((\f' -> f' x) f)) (\a -> a)
--        = \f x -> (\f'' -> f'' (f x))) (\a -> a)
--        = \f x -> (\a -> a) (f x)
--        = \f x -> f x
--        = one

-- minus won't work

newtype Church = Church {runChurch :: forall a. (a -> a) -> a -> a}

predC :: Church -> Church
predC n = Church $ \f x -> runChurch n (\n' f' -> f' (n' f)) (\_ -> x) (\a -> a)

minus :: Church -> Church -> Church
minus a b = runChurch b predC a

toIntC :: Church -> Int
toIntC n = runChurch n (+ 1) 0

-- Booleans

true = \a b -> a

false = \a b -> b

toBool p = p True False

ifthen = \p a b -> p a b

and = \p q -> p q false

or = \p q -> p true q