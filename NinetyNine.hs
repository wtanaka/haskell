#!/usr/bin/env runhaskell
--
-- Copyright 2014 Wesley Tanaka <http://wtanaka.com/>
--
-- This file is part of https://github.com/wtanaka/haskell
--
-- https://github.com/wtanaka/haskell is free software: you can
-- redistribute it and/or modify it under the terms of the GNU General
-- Public License as published by the Free Software Foundation,
-- either version 3 of the License, or (at your option) any later
-- version.
--
-- https://github.com/wtanaka/haskell is distributed in the hope that
-- it will be useful, but WITHOUT ANY WARRANTY; without even the
-- implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
-- PURPOSE.  See the GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with https://github.com/wtanaka/haskell .  If not, see
-- <http://www.gnu.org/licenses/>.
--
-- http://www.haskell.org/haskellwiki/99_questions/1_to_10
module NinetyNine(myLast,
   myButLast,
   elementAt,
   myLength,
   myReverse,
   isPalindrome,
   flatten,
   compress,
   pack,
   encode,
   encodeModified,
   decodeModified,
   encodeDirect,
   dupli,
   repli,
   dropEvery,
   split,
   slice,
   _plusRem,
   rotate,
   removeAt,
   insertAt,
   range,
   rnd_select,
   diff_select,
   combinations,
   isPrime,
   myGCD,
   coprime,
   totient,
   primeFactors,
   prime_factors_mult,
   totient2,
   primesR,
   goldbach,
   goldbachList,
   goldbachList',
   and',
   or',
   nand',
   nor',
   xor',
   impl',
   equ',
   table,
   tablen,
   gray,
   cbalTree,
   symmetric,
) where

import Control.Arrow
import Data.List
import qualified Data.Set as Set
import System.Random (RandomGen, Random, randomR, getStdGen, setStdGen)

-- Problem 1
myLast :: [a] -> a
myLast [] = error "Empty list has no last element"
myLast (x : []) = x
myLast (_ : xs) = myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast [] = error "Empty list has no second to last element"
myButLast (_ : []) = error "One-item list has no second to last element"
myButLast (x : _ : []) = x
myButLast x = myButLast (tail x)

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt [] _ = error "Empty list has no elements"
elementAt (x : _) 0 = x
elementAt [_] _ = error "Index out of bounds"
elementAt (_ : xs) number = elementAt xs (number-1)

-- Problem 4
increment :: Int -> a -> Int
increment x _ = x + 1
myLength :: [a] -> Int
myLength = foldl' increment 0

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x : xs)) = flatten x ++ flatten (List xs)

-- Problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x : y : zs) = if x == y
   then compress (y : zs)
   else x : compress (y : zs)

-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x : zs) = let packzs = pack zs in
   let headhead = head (head packzs) in
      if x == headhead
      then (x : head packzs) : tail packzs
      else [x] : packzs

-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode [x] = [(1,  x)]
encode (x : xs) = let encodexs = encode xs in
   if x == snd (head encodexs)
   then (1 + fst (head encodexs), x) : tail encodexs
   else (1, x) : encodexs

-- Problem 11
data SingleOrMultiple a = Single a | Multiple Int a deriving (Show)
encodeModified :: Eq a => [a] -> [SingleOrMultiple a]
encodeModified [] = []
encodeModified [x] = [Single x]
encodeModified (x : xs) = let encodedxs = encodeModified xs in
   case head encodedxs of
      Single z -> if x == z
         then Multiple 2 x : tail encodedxs
         else Single x : encodedxs
      Multiple count z -> if x == z
         then Multiple (1+count) x : tail encodedxs
         else Single x : encodedxs

-- Problem 12
decodeModified :: [SingleOrMultiple a] -> [a]
decodeModified [] = []
decodeModified (x : xs) = let decodedtail = decodeModified xs in
   case x of
      Single z -> z : decodedtail
      Multiple count z -> replicate count z ++ decodedtail

-- Problem 13
encodeDirect :: Eq a => [a] -> [SingleOrMultiple a]
encodeDirect = encodeModified

-- Problem 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x : xs) = x : (x : dupli xs)

-- Problem 15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x : xs) count = replicate count x ++ repli xs count

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery list count = dropHelper list count (count-1)
dropHelper :: [a]
   -> Int -- period
   -> Int -- skip
   -> [a]
dropHelper [] _ _ = []
dropHelper (_ : xs) period 0 = dropHelper xs period (period-1)
dropHelper (x : xs) period skip = x : dropHelper xs period (skip-1)

-- Problem 17
split :: [a] -> Int -> ([a], [a])
split [] 0 = ([], [])
split [] _ = error "Not enough elements"
split list 0 = ([], list)
split (x : xs) count = let splittail = split xs (count-1) in
   (x : fst splittail, snd splittail)

-- Problem 18
slice :: [a]
   -> Int -- start
   -> Int -- end
   -> [a]
slice _ start _ | start < 1 = error "Non-positive start"
slice [] _ _ = error "Out of bounds"
slice (x : _) 1 1 = [x]
slice (x : xs) 1 end = x : slice xs 1 (end-1)
slice (_ : xs) start end = slice xs (start-1) (end-1)

-- Problem 19
_plusRem :: Int -> Int -> Int
_plusRem num denom = rem nonnegative denom
   where mightbenegative = rem num denom
         nonnegative = if mightbenegative < 0
            then mightbenegative + abs denom
            else mightbenegative

rotate :: [a] -> Int -> [a]
rotate list count = let listlength = length list in
   rotateHelper list listlength (_plusRem count listlength)
rotateHelper :: [a] -> Int -> Int -> [a]
rotateHelper [] _ _ = []
rotateHelper list _ 0 = list
rotateHelper list listlength count =
   slice list (count+1) listlength ++ slice list 1 count

-- Problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt _ [] = error "Can't remove from empty list"
removeAt 1 (x : xs) = (x, xs)
removeAt index (x : xs) = let (removed, remainder) = removeAt (index-1) xs
   in (removed, x : remainder)

-- Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt thing [] 1 = [thing]
insertAt _ [] _ = error "Can't insert into empty list"
insertAt thing list 1 = thing : list
insertAt thing (x : xs) pos = x : insertAt thing xs (pos-1)

-- Problem 22
range :: Int -> Int -> [Int]
range x y
   | x == y = [x]
   | x < y = x : range (x+1) y
   | otherwise = error "arguments out of order"

-- Problem 23
-- Generate N random numbers in the range (a, a) with replacement
replicateRandomR :: (RandomGen g, Random a) => Int -> (a, a) -> g -> ([a], g)
replicateRandomR 0 _ gen = ([], gen)
replicateRandomR count range gen = let
   (as, g1) = replicateRandomR (count-1) range gen
   (a, g2) = randomR range g1
   in (a : as, g2)

rndSelect :: [a] -> Int -> IO [a]
rndSelect list count = do
   gen <- getStdGen
   let (indexes, newGen) = replicateRandomR count (0, length list - 1) gen
      in do
         setStdGen newGen
         return [ list !! i | i <- indexes ]

rnd_select = rndSelect

-- Problem 24
-- Generate N random numbers in the range (a, a) without replacement
nrRepicateRandomRHelper :: (RandomGen g, Ord a, Random a)
   => Int -> (a, a) -> Set.Set a -> g -> (Set.Set a, g)
nrRepicateRandomRHelper 0 _ selectedIndexes gen = (selectedIndexes, gen)
nrRepicateRandomRHelper count range selectedIndexes gen = let
   (newIndex, gen2) = randomR range gen
   in if Set.member newIndex selectedIndexes
         then nrRepicateRandomRHelper count range selectedIndexes gen2
         else nrRepicateRandomRHelper (count-1) range (Set.insert newIndex
            selectedIndexes) gen2

diffSelectPure :: (RandomGen g) => Int -> Int -> g -> ([Int], g)
diffSelectPure count max gen = let
   (results, gen2) = nrRepicateRandomRHelper count (0, max-1) Set.empty gen
   in ([ [1..max] !! i | i <- Set.toList results ], gen2)

diffSelect :: Int -> Int -> IO [Int]
diffSelect count max = do
   gen <- getStdGen
   let (results, newGen) = diffSelectPure count max gen
      in do
         setStdGen newGen
         return results

diff_select = diffSelect

-- Problem 25
-- Generate a random permutation of the elements of a list.
-- Skipping for now

-- Problem 26
prependToAll :: a -> [[a]] -> [[a]]
prependToAll _ [] = []
prependToAll a (l : ls) = (a : l) : prependToAll a ls

-- Generate the combinations of K distinct objects chosen from the N
-- elements of a list
combinations :: Int -> [a] -> [[a]]
combinations _ [] = [[]]
combinations 0 _ = [[]]
combinations 1 as = map (:[]) as
combinations num list
   | num > len = []
   | otherwise = prependToAll a (combinations (num - 1) as)
      ++ combinations num as
   where
      len = length list
      a = head list
      as = tail list

-- Problem 31
isPrime :: Int -> Bool
isPrime n | n < 1 = error "only positive numbers accepted"
isPrime 1 = False
isPrime 2 = True
isPrime n = let ceil = ceiling (sqrt (fromIntegral n))
   in notElem 0 [n `rem` k | k <- [2..ceil]]

-- Problem 32
myGCDHelper :: Int -> Int -> Int
myGCDHelper a 0 = a
myGCDHelper a b = myGCD b (a `rem` b)

myGCD :: Int -> Int -> Int
myGCD a b = abs (myGCDHelper a b)

-- Problem 33
coprime :: Int -> Int -> Bool
coprime a b = 1 == myGCD a b

-- Problem 34
totient :: Int -> Int
totient 1 = 1
totient n = length . filter (coprime n) $ [1..n-1]

-- Problem 35
primeFactorHelper :: [Int] -> Int -> [Int]
primeFactorHelper _ n
   | n < 1 = error "only supports positive numbers"
primeFactorHelper listSoFar 2 = 2 : listSoFar
primeFactorHelper listSoFar n = let
   ceil = ceiling (sqrt (fromIntegral n))
   factor = take 1 [k | k <- [2..ceil], n `rem` k == 0 && isPrime k]
   in if null factor
      then n : listSoFar
      else primeFactorHelper (head factor : listSoFar) (n `quot` head factor)

primeFactors :: Int -> [Int]
primeFactors = reverse . primeFactorHelper []

-- Problem 36
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult = map (snd Control.Arrow.&&& fst) . encode . primeFactors

prime_factors_mult = primeFactorsMult

-- Problem 37
totient2 :: Int -> Int
totient2 1 = 1
totient2 n = product [(p - 1) * p ^ (m-1) | (p, m) <- prime_factors_mult n]

-- Problem 38
--
--    Wed Sep 24 09:17 2014 Time and Allocation Profiling Report  (Final)
--
--       ProblemThirtyEight +RTS -p -RTS
--
--    total time  =        0.01 secs   (10 ticks @ 1000 us, 1 processor)
--    total alloc =   3,988,384 bytes  (excludes profiling overheads)
--
--                individual     inherited
-- COST CENTRE   %time %alloc   %time %alloc
--
--    totient     20.0   13.0   100.0   98.7
--    totient2     0.0    0.0     0.0    0.2

-- Problem 39
primesR :: Int -> Int -> [Int]
primesR low high
   | low > high = error "low limit must be at most equal to high limit"
   | low < 1 || high < 1 = error "inputs must be positive"
   | otherwise = [n | n <- [low..high], isPrime n]

-- Problem 40
goldbach :: Int -> (Int, Int)
goldbach n
   | odd n = error "n must be even"
   | n <= 2 = error "n must be greater than 2"
   | otherwise = let
      ceil = quot n 2
      in head $ filter (isPrime . snd) [(k, n - k) | k <- [2..ceil], isPrime k]

-- Problem 41
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList a b = [goldbach n | n <- [a..b], even n]

bothGreaterThan :: Int -> (Int, Int) -> Bool
bothGreaterThan threshold (a, b) = a > threshold && b > threshold

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' low high threshold =
   filter (bothGreaterThan threshold) $ goldbachList low high

-- Problem 46
and' = (&&)
or' = (||)
nand' a b = not (a && b)
nor' a b = not (a || b)
xor' a b = (a && not b) || (b && not a)
impl' a b = not (a && not b)
equ' a b = (a && b) || (not a && not b)

-- [a]^n gives the cross product of [a] with itself n times
listExp :: [a] -> Int -> [[a]]
listExp _ 0 = []
listExp list 1 = map (:[]) list
listExp list count =
   concat [[x : perm | perm <- listExp list (count-1)] | x <- list]

tableLine :: (Bool -> Bool -> Bool) -> Bool -> Bool -> String
tableLine f x y = show x ++ " " ++ show y ++ " " ++ show (f x y) ++ "\n"

table :: (Bool -> Bool -> Bool) -> String
table function =
   foldl (++) "" [tableLine function x y |
      (x : y : []) <- listExp [True, False] 2]

-- Problem 47
infixl 2 `or'`
infixl 2 `nor'`
infixl 3 `and'`
infixl 3 `nand'`
infixl 3 `xor'`
infixl 3 `impl'`
infixl 4 `equ'`

-- Problem 48
tableLinen :: ([Bool] -> Bool) -> [Bool] -> String
tableLinen f x = concat [show a ++ " " | a <- x] ++ show (f x) ++ "\n"

tablen :: Int -> ([Bool] -> Bool) -> String
tablen nterms function =
   foldl (++) "" [tableLinen function x |
      x <- listExp [True, False] nterms]

-- Problem 49
gray :: Int -> [String]
gray = listExp "01"

-- Problem 55
data Tree a = Empty | Branch a (Tree a) (Tree a)
   deriving (Show, Eq)

leaf :: x -> Tree x
leaf x = Branch x Empty Empty

cbalTree :: Int -> Tree Char
cbalTree 0 = Empty
cbalTree 1 = leaf 'x'
cbalTree n = let
   thequot = quot (n-1) 2
   in if even (n-1)
      then Branch 'x' (cbalTree thequot) (cbalTree thequot)
      else Branch 'x' (cbalTree (thequot+1)) (cbalTree thequot)

-- Problem 56
mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror Empty _ = False
mirror _ Empty = False
mirror (Branch _ ll lr) (Branch _ rl rr) = mirror ll rr && mirror lr rl

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ ll lr) = mirror ll lr
