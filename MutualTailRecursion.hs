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

module Main(main) where

import qualified Control.Monad.State.Strict as Strict

-- Bad
countABsBad :: String -> (Int, Int)
countABsBad [] = (0, 0)
countABsBad (x : xs) = case x of
   'a' -> let (as, bs) = countABsBad xs in (as+1, bs)
   'b' -> let (as, bs) = countABsBad xs in (as, bs+1)
   _ -> countABsBad xs


_incFirst :: (Int, Int) -> (Int, Int)
_incFirst x = let sum = 1 + fst x in seq sum (sum, snd x)

_incSecond :: (Int, Int) -> (Int, Int)
_incSecond x = let sum = 1 + snd x in seq sum (fst x, sum)

-- Good
_countABsMonad :: String -> Strict.State (Int, Int) ()
_countABsMonad [] = return ()
_countABsMonad (x : xs) = do
   case x of
      'a' -> do
         s <- Strict.get
         Strict.put $! _incFirst s
      'b' -> do
         s <- Strict.get
         Strict.put $! _incSecond s
      _ -> return ()
   _countABsMonad xs

countABsMonad :: String -> (Int, Int)
countABsMonad xs = Strict.execState (_countABsMonad xs) (0, 0)

_countAMonad :: String -> Strict.State Int ()
_countAMonad [] = return ()
_countAMonad (x : xs) = do
   case x of
      'a' -> do
         s <- Strict.get
         Strict.put $! 1 + s
      _ -> return ()
   _countAMonad xs

countAsMonad :: String -> Int
countAsMonad xs = Strict.execState (_countAMonad xs) 0

-- Mutual Recursion
_countNonAsMutual1 :: String -> Int
_countNonAsMutual1 [] = 0
_countNonAsMutual1 xs
   | 'a' == head xs = countAsMutual1 xs
   | otherwise = _countNonAsMutual1 $ tail xs

countAsMutual1 :: String -> Int
countAsMutual1 [] = 0
countAsMutual1 ('a' : xs) = 1 + countAsMutual1 xs
countAsMutual1 xs = _countNonAsMutual1 xs

-- Mutual Tail Recursion
_countNonAsMutual2 :: String -> Int -> Int
_countNonAsMutual2 [] acc = acc
_countNonAsMutual2 xs acc
   | 'a' == head xs = countAsMutual2 xs acc
   | otherwise = _countNonAsMutual2 (tail xs) acc

countAsMutual2 :: String -> Int -> Int
countAsMutual2 [] acc = acc
countAsMutual2 ('a' : xs) acc = countAsMutual2 xs (acc+1)
countAsMutual2 xs acc = _countNonAsMutual2 xs acc

asbs = 'a' : 'b' : asbs

short = take 10 asbs

long = take 1999999 asbs

longhelp [] _ = []
longhelp _ 0 = []
longhelp (x : xs) n = x : longhelp xs (n-1)

main :: IO ()
main = do
   -- print $ long' !! 1999999
   print $ countABsBad short
   -- stack overflow
   -- print $ countABsBad long
   print $ countAsMonad short
   print $ countAsMonad (longhelp asbs 19999999)
   print $ countABsMonad short
   print $ countABsMonad long
   print $ countABsMonad (longhelp asbs 19999999)
   print $ countAsMutual1 short
   -- stack overflow
   -- print $ countAsMutual1 (longhelp asbs 19999999)
   print $ countAsMutual2 short 0
   print $ countAsMutual2 (longhelp asbs 19999999) 0
