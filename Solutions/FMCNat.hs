{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , ($)
    , (.)
    , (++)
    , undefined
    , error
    , otherwise
    ,String
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show :: Nat -> String
    show O = "O"
    show (S n) = "S" ++ show n

instance Eq Nat where

    (==) :: Nat -> Nat -> Bool
    O == O = True
    (S n) == (S m) = n == m
    _ == _ = False

instance Ord Nat where

    (<=) :: Nat -> Nat -> Bool
    O <= _ = True
    (S _) <= O = False
    (S n) <= (S m) = n <= m

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min :: Nat -> Nat -> Nat
    min O _ = O
    min (S n) (S m) = min n m

    max :: Nat -> Nat -> Nat
    max O n = n
    max (S n) (S m) = max n m


----------------------------------------------------------------
-- some sugar
----------------------------------------------------------------

zero, one, two, three, four, five, six, seven, eight :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven

----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = True
isZero (S _) = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S n) = n

even :: Nat -> Bool
even O = True
even (S O) = False
even (S (S n)) = even n

odd :: Nat -> Bool
odd O = False
odd (S n) = even n


----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
n <+> O = n
n <+> S m = S (n <+> m)

infixl 6 <+>

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus O  _ = O
monus n O = n
monus (S n) (S m) = monus n m

(<->) :: Nat -> Nat -> Nat
(<->) = monus

-- multiplication
times :: Nat -> Nat -> Nat
times O  _ = O
times (S n) m = times n m <+> m

(<*>) :: Nat -> Nat -> Nat
(<*>) = times

infixl 7 <*>

-- power / exponentiation
pow :: Nat -> Nat -> Nat
pow _  O = S O
pow n  (S m) = pow n m <*> n

exp :: Nat -> Nat -> Nat
exp _  O = S O
exp n  (S m) = exp n m <*> n

(<^>) :: Nat -> Nat -> Nat
(<^>) = pow -- ou <^> = exp

infixr 8 <^>

-- quotient
(</>) :: Nat -> Nat -> Nat
_ </> O = error "division by O"
n </> (S m)
    |(n <-> S m) == O && (S m <-> n) == O = S O
    |(n <-> S m) == O = O
    |otherwise = S ((n <-> S m) </> S m)


-- remainder
(<%>) :: Nat -> Nat -> Nat
O <%> _ = O
_ <%> O = error "divion by O"
n <%> (S m) = n <-> ((n </> S m) <*> S m)


-- euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucidiv (_, O) = error "division by O"
eucdiv (n, S m)
    |n < S m = (O, n)
    |otherwise =
    let (q, r) = eucdiv (n <-> S m, S m)
    in (S q, r)


-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) _ O = True   -- two divides zero - S O
(<|>) O _ = False     -- zero divides five - O
(<|>) n (S m)
    |S m <%> n == O = True
    |otherwise = False

divides :: Nat -> Nat -> Bool
divides = (<|>)


-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)
dist :: Nat -> Nat -> Nat
dist n O = n
dist O n = n
dist (S n) (S m) = dist n m

(|-|) :: Nat -> Nat -> Nat
(|-|) = dist

factorial :: Nat -> Nat
factorial O = S O
factorial (S n) = factorial n * S n

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O
sg (S _) = S O

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo O _ = error "logarithm base zero"
lo _ O = error "logarithm of O"
lo (S O) _ = error "logarithm base one"
lo n (S m)
    |S m <-> n == O && n <-> S m == O = S O
    |S m <-> n == O = O
    |otherwise = S (lo n (S m </> n))


----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat 0 = O
toNat n
    |n>0 = S (toNat (n-1))
    |otherwise = error "number < 0"

fromNat :: Integral a => Nat -> a
fromNat O = 0
fromNat (S m) = fromNat m + 1


-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where
    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
      | x < 0     = error "number < 0"
      | x == 0    = O
      | otherwise = S (toNat (x-1))

