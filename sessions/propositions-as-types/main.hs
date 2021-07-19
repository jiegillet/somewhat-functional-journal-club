import Data.Void

modus_tollens :: ((a -> b), (b -> Void)) -> (a -> Void)
modus_tollens = \p x -> (snd p) ((fst p) x)


modus_tollens' :: ((a -> b), (b -> Void)) -> (a -> Void)
modus_tollens' (f, g) = g . f


