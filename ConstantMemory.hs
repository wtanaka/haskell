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
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Main(main) where

asbs = 'a' : 'b' : asbs

largenum = 10000000

long = take largenum asbs
varlong n = take n asbs

-- O(1) memory
-- test = print $ long !! (largenum-1)

-- O(1) memory
-- test = print $ length long

-- O(1) memory with -O2 O(N) memory without
--test = do
--   print $ long !! 9999999
--   print $ length long

-- O(1) memory without -O2, O(N) memory with -O2
--test = do
--   print $ varlong largenum !! (largenum-1)
--   print $ length $ varlong largenum

-- O(1) memory without -O2, O(N) memory with -O2
test = do
   print $ take 10 (varlong largenum)
   print $ varlong largenum !! (largenum-1)
   print $ length $ varlong largenum

main :: IO ()
main = test
