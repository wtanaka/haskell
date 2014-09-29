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

import Data.Char (ord)
import Data.Monoid (mappend)

import qualified Data.ByteString.Lazy as BSL (
   ByteString,
   count,
   interact,
   )

import qualified Data.ByteString.Lazy.Builder as BSB (
   toLazyByteString,
   string7,
   char7,
   )

_lf = fromIntegral (ord '\n')

toByteString :: (Show a) => a -> BSL.ByteString
toByteString a = BSB.toLazyByteString
   $ BSB.string7 (show a) `mappend` BSB.char7 '\n'

wc :: BSL.ByteString -> BSL.ByteString
wc bs = toByteString $ BSL.count _lf bs

main :: IO ()
main = BSL.interact wc
