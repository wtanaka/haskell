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
) where

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
myLength :: [a] -> Int
myLength [] = 0
myLength (x : xs) = 1 + myLength xs

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = (xs == (reverse xs))

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x : xs)) = flatten x ++ (flatten (List xs))

-- Problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x : y : zs) = if x == y
   then compress (y : zs)
   else [x] ++ compress (y : zs)

-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x : zs) = let packzs = pack zs in
   let headhead = head (head packzs) in
      if x == headhead
      then ((x : head packzs) : tail packzs)
      else ([x] : packzs)

-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode [x] = [(1,  x)]
encode (x : xs) = let encodexs = encode xs in
   if x == snd (head encodexs)
   then ((1 + fst (head encodexs), x) : tail encodexs)
   else ((1, x) : encodexs)

-- Problem 11
data SingleOrMultiple a = Single a | Multiple Int a deriving (Show)
encodeModified :: Eq a => [a] -> [SingleOrMultiple a]
encodeModified [] = []
encodeModified [x] = [Single x]
encodeModified (x : xs) = let encodedxs = encodeModified xs in
   case head encodedxs of
      Single z -> if x == z
         then (Multiple 2 x : tail encodedxs)
         else (Single x : encodedxs)
      Multiple count z -> if x == z
         then (Multiple (1+count) x : tail encodedxs)
         else (Single x : encodedxs)
