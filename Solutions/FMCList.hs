{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use isAsciiUpper" #-}
{-# HLINT ignore "Use map" #-}

module FMCList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C
import Data.Char (toLower)
import Text.Read (Lexeme(Char))

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}


{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

head :: [a] -> a
head [] = error "Nil"
head (x:_) = x

tail :: [a] -> [a]
tail (_: xs) = xs
tail [] = error "Nil"

null :: [a] -> Bool
null [] = True
null (_:_) = False

length :: Integral i => [a] -> i
length [] = 0
length (_:xs) = length xs + 1

sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

product :: Num a => [a] -> a
product [] = 0
product (x:xs) = x * product xs

reverse :: [a] -> [a]
reverse [] = []
reverse xs = reverse (tail xs) ++ [head xs]

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc x (y:ys) = y:snoc x ys

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?!)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum [x] = x
minimum (x:xs)
  | x < minimum xs = x
  |otherwise = minimum xs

maximum :: Ord a => [a] -> a
maximum [x] = x
maximum (x:xs)
  | x > minimum xs = x
  |otherwise = minimum xs

take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take i (x:xs) = x: take (i-1) xs

drop :: Int -> [a] -> [a]
drop _ [] = []
drop 0 xs = xs
drop i (_:xs) = drop (i-1) xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile f (x:xs)
  |f x = x:takeWhile f xs
  |otherwise = []

dropWhile :: (b -> Bool) -> [b] -> [b]
dropWhile _ [] = []
dropWhile f (x:xs)
  |f x = dropWhile f xs
  |otherwise = x:xs

tails :: [a] -> [[a]] --todas as caudas/rabos
tails [] = [[]]
tails (_:xs) = xs :tails (tail xs) 

init :: [a] -> [a]
init [] = error "nil"
init [x] = []
init (x:xs) = x:init xs

inits :: [a] -> [[a]]
inits [] = [[]]
inits xs = inits (init xs) ++ [xs]
  --L.inits [1,2,2,4]  [[],[1],[1,2],[1,2,2],[1,2,2,4]]

subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) =  subsequences xs ++ map (x:) (subsequences xs)

any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any f (x:xs) = f x || any f xs

all :: (a -> Bool) -> [a] -> Bool
all _ [] = True --nehuma contradição
all f (x:xs) = f x && all f xs

and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

or :: [Bool] -> Bool
or [] = False
or (x:xs) = x || or xs

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs

-- elem using the funciton 'any' above
elem :: Eq a => a -> [a] -> Bool
elem y = any (==y)

-- elem': same as elem but elementary definition
-- (without using other functions except (==))
elem' :: Eq a => a -> [a] -> Bool
elem' y (x:xs) = (y == x) || elem y xs

(!!) :: [a] -> Int -> a
[] !! _ = error "Nil"
(x:_) !! 0= x 
(_:xs) !! i = xs !! (i-1)

filter :: (a-> Bool) -> [a] -> [a]
filter f [] = []
filter f (x:xs)
  |f x = x:filter f xs
  |otherwise = filter f xs

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

cycle :: [a] -> [a]
cycle []= error "Nil"
cycle xs = xs ++ cycle xs

repeat :: a -> [a]
repeat x = x :repeat x

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate i x = x:replicate (i-1) x

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = (x==y) && isPrefixOf xs ys

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf xs ys 
  |length xs > length ys = False 
  |isPrefixOf xs ys = True
  |otherwise = isInfixOf xs (tail ys) -- vai para o proximo

isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf xs ys = isPrefixOf (reverse xs) (reverse ys)

zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y):zip xs ys

-- zipWith

intercalate :: [a] -> [[a]] -> [a]
intercalate _ [] = []
intercalate _[x] = x
intercalate ys (x:xs) = x ++ ys ++ intercalate ys xs

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x:nub(filter (/=x) xs)

splitAt :: Int -> [a] -> ([a], [a])
splitAt 0 xs = ([], xs)
splitAt _ [] = ([],[])
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

break :: (a-> Bool) -> [a] -> ([a],[a])
break _ [] = ([], [])

-- lines
-- words
-- unlines
-- unwords

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:_) = []
transpose xs = map head xs : transpose (map tail xs)

-- checks if the letters of a phrase form a palindrome (see below for examples)
isAlpha:: Char -> Bool
isAlpha c = elem c (['A' .. 'Z'] ++ ['a' .. 'z'])

palindrome :: String -> Bool
palindrome [] =  True
palindrome [_] = True
palindrome xs = map toLower (filter isAlpha xs) == reverse (map toLower (filter isAlpha xs))

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

