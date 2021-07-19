import Prelude hiding (succ, pred, and, or)

zero = \f x -> x
one = \f x -> f x
two = \f x -> f (f x)
three = \f x -> f (f (f x))

num 0 = zero
num n = \f x -> f (num (n-1) f x)

toInt n = n (+1) 0
toUnits n = n (() :)  []
toLaughs n = n (++"ha") ""

succ n = \f x -> f (n f x)
add a b = \f x -> b f (a f x)
mult a b = \f x -> a (b f) x
pow b e = \f x -> e b f x

pred n = \f x -> n (\n' f' -> f' (n' f)) 
            (\_ -> x) (\a -> a)

minus a b = b pred a
