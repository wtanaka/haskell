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
) where

import Data.List

-- Problem 1
myLast :: [a] -> a
myLast [] = error "Empty list has no last element"
myLast (x : []) = x
myLast (x : xs) = myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast [] = error "Empty list has no second to last element"
myButLast (x : []) = error "One-item list has no second to last element"
myButLast (x : y : []) = x
myButLast x = myButLast (tail x)

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt [] _ = error "Empty list has no elements"
elementAt (x : _) 0 = x
elementAt [x] _ = error "Index out of bounds"
elementAt (x : xs) number = elementAt xs (number-1)

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
dropHelper (x : xs) period 0 = dropHelper xs period (period-1)
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
slice (x : xs) 1 1 = [x]
slice (x : xs) 1 end = x : slice xs 1 (end-1)
slice (x : xs) start end = slice xs (start-1) (end-1)

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


