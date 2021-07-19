import Prelude
%hide Nat
%hide Maybe

data Nat = Z | S Nat

add : Nat -> Nat -> Nat
add Z m = m
add (S n) m = S (add n m)

mul : Nat -> Nat -> Nat
mul Z m = Z
mul (S n) m = add m (mul n m)


-- Extended notation for type declaration
data Nat' : Type where
  Z' : Nat'
  S' : Nat' -> Nat'

data Maybe a = Nothing | Just a

data Maybe' : Type -> Type where
  Nothing' : Maybe' a
  Just' : a -> Maybe' a

-- Expression which returns a type
f : Bool -> Type
f True = Int
f False = String

-- Dependent function
g : (n : Bool) -> f n
g True = 0
g False = "Hello"

-- Implicit arguments
myId : (a : Type) -> a -> a
myId a x = x

myId' : {a : Type} -> a -> a
myId' x = x

myId'' : a -> a
myId'' x = x

-- Useful example
data List' : Type -> Type where
  Nil' : List' a
  Cons' : a -> List' a -> List' a

data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  Cons : a -> Vect n a -> Vect (S n) a

head : Vect (S n) a -> a
head (Cons x _) = x
-- this is not mandatory
head Nil impossible

concat : Vect n a -> Vect m a -> Vect (add n m) a
concat Nil xs = xs
concat (Cons x xs) ys = Cons x (concat xs ys)

length : {n : Nat} -> Vect n a -> Nat
length {n} _ = n

-- The Identity type
--data (=) : a -> b -> Type where
--  Refl : x = x

twoPlusTwo : 2 + 2 = 4
twoPlusTwo = Refl

cong' : {f : t -> u} -> x = y -> f x = f y
cong' Refl = Refl

sym' : (x = y) -> (y = x)
sym' Refl = Refl

trans' : (x = y) -> (y = z) -> (x = z)
trans' Refl Refl = Refl


-- Proofs

total
addReducesS : (n : Nat) -> (m : Nat) -> S (add n m) = add n (S m)
addReducesS Z m = Refl
addReducesS (S k) m = cong' (addReducesS k m)


total
notEqual : 2 + 2 = 5 -> Void
notEqual Refl impossible


-- Another cool type
data Elem : List a -> a -> Type where
  Here : Elem (t :: _) t
  There : Elem ts t -> Elem (_ :: ts) t

total
emptyListIsEmpty : Elem [] _ -> Void
emptyListIsEmpty Here impossible
emptyListIsEmpty There impossible


-- Dependent Pair
data DPair' : (a : Type) -> (a -> Type) -> Type where
  MkDPair' : {P : a -> Type} -> (x : a) -> P x -> DPair' a P

-- Notation **
d : DPair Bool Main.f
d = MkDPair False "Hi"

d' : (b : Bool ** f b)
d' = (False ** "Hi")

-- Type of even numbers
T : Type
T = (x : Nat ** add x x = S (S Z))

verifiedSolution : T
verifiedSolution = (S Z ** Refl)

-- Prove no solutions exist
T2 : Type
T2 = (x : Nat ** mul x x = S (S Z))

total
t2IsFalse : T2 -> Void
t2IsFalse (Main.Z ** Refl) impossible


