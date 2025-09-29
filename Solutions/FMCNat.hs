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
(<^>) = pow

infixr 8 <^>

-- quotient
(</>) :: Nat -> Nat -> Nat
_ </> O = undefined
n </> (S m)
    |(n <-> S m) == O && (S m <-> n) == O = S O
    |(n <-> S m) == O = O
    |otherwise = S ((n <-> S m) </> S m)
    

-- remainder
(<%>) :: Nat -> Nat -> Nat
O <%> _ = O
_ <%> O = undefined
n <%> (S m) = n <-> ((n </> S m) <*> S m)

-- euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucdiv = undefined
-- **fazer************************************** --

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
lo O _ = undefined
lo _ O = undefined
lo (S O) _ = undefined
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
toNat = undefined

fromNat :: Integral a => Nat -> a
fromNat = undefined


-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) :: Nat -> Nat -> Nat
    (+) = (<+>)

    (*) :: Nat -> Nat -> Nat
    (*) = (<*>)

    (-) :: Nat -> Nat -> Nat
    (-) = (<->)

    abs :: Nat -> Nat
    abs n = n

    signum :: Nat -> Nat
    signum = sg

    fromInteger x
      | x < 0     = undefined
      | x == 0    = undefined
      | otherwise = undefined

