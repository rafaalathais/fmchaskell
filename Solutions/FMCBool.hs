module ExBool where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Enum(..)
    , Integral(..)
    , Int
    , Char
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    , String
    )
import System.Win32 (xBUTTON1)

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Bool = False | True

instance Show Bool where

    show :: Bool -> String
    show True = "tt"
    show False = "ff"

instance Enum Bool where

    toEnum :: Int -> Bool
    toEnum 0 = False
    toEnum 1 = True

    fromEnum :: Bool -> Int
    fromEnum True = 1
    fromEnum False = 0

-- conjunction (AND)
(&&) :: Bool -> Bool -> Bool
True && True = True
_ && _ = False

infixr 3 &&

-- disjunction (OR)
(||) :: Bool -> Bool -> Bool
False || False = False
_ || _ = True

infixr 2 ||

-- NAND (Sheffer stroke)
(/|\) :: Bool -> Bool -> Bool
False /|\ _ = True
True /|\ True = False
True /|\ False = True

infixr 2 /|\

-- NOR (aka: Peirce arrow or Quine dagger)
(\|/) :: Bool -> Bool -> Bool
False \|/ False = True
True \|/ _ = False
_ \|/ True = False


infixr 2 \|/

-- XOR (exclusive disjunction)
(<=/=>) :: Bool -> Bool -> Bool
True  <=/=> False = True
False <=/=> True  = True
_  <=/=>  _  = False

infixr 2 <=/=>

-- boolean negation
not :: Bool -> Bool
not True = False
not False = True

-- if-then-else expression
ifThenElse :: Bool -> a -> a -> a
ifThenElse True x y = x
ifThenElse False x y = y 

-- logical "implies"
(==>) :: Bool -> Bool -> Bool
x ==> y = not x || y

infixr 1 ==>

-- logical "implied by"
(<==) :: Bool -> Bool -> Bool
x <== y = y ==> x

infixl 1 <==

-- logical equivalence
(<=>) :: Bool -> Bool -> Bool
x <=> y = (x ==> y) && (y ==> x)

infixr 1 <=>


