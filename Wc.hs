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

import Data.ByteString.Lazy.Builder (Builder)
import Data.Char (ord)
import Data.Int (Int64)
import Data.List (sort)
import Data.Monoid (mappend)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Builder as BSB
import qualified Data.Word8 as Word8
import qualified System.Console.GetOpt as GetOpt
import System.Environment
import System.IO
import Text.Printf

-- Represents a single command line option
data Option =
   Bytes
   | Lines
   | Words
   | Help
   deriving (Show, Eq)

-- Option requested -- should be one of
data OutputOption =
   OutputBytes
   | OutputLines
   | OutputWords
   deriving (Show, Eq)

_optionOrdMap OutputLines = 0
_optionOrdMap OutputWords = 1
_optionOrdMap OutputBytes = 2

instance Ord OutputOption where
   compare a b = compare (_optionOrdMap a) (_optionOrdMap b)

_optionFunctionMap :: OutputOption -> BSL.ByteString -> Int64
_optionFunctionMap OutputLines = numLines
_optionFunctionMap OutputBytes = numBytes
_optionFunctionMap OutputWords = numWords

_lf = fromIntegral (ord '\n')

_optDescr :: [GetOpt.OptDescr Option]
_optDescr = [
   GetOpt.Option "h" ["help"] (GetOpt.NoArg Help)
      "display this help and exit"
   , GetOpt.Option "l" ["lines"] (GetOpt.NoArg Lines)
      "print the newline counts"
   , GetOpt.Option "c" ["bytes"] (GetOpt.NoArg Bytes)
      "print the byte counts"
   , GetOpt.Option "w" ["words"] (GetOpt.NoArg Words)
      "print the word counts"
   ]

appendEol :: Builder -> Builder
appendEol x = x `mappend` BSB.char7 '\n'

numLines :: BSL.ByteString -> Int64
numLines = BSL.count _lf

numBytes :: BSL.ByteString -> Int64
numBytes = BSL.length

_numWordsStartsWithSpace :: BSL.ByteString -> Int64
_numWordsStartsWithSpace bs = let
   firstWordIdx = BSL.findIndex (not . Word8.isSpace) bs
      in case firstWordIdx of
         Nothing -> 0
         Just idx -> _numWordsStartsWithNonSpace $ BSL.drop idx bs

_numWordsStartsWithNonSpace :: BSL.ByteString -> Int64
_numWordsStartsWithNonSpace bs
   | BSL.null bs = 0
   | otherwise = let firstSpaceIdx = BSL.findIndex Word8.isSpace bs
      in case firstSpaceIdx of
         Nothing -> 1
         Just idx -> 1 + _numWordsStartsWithSpace (BSL.drop idx bs)

numWords :: BSL.ByteString -> Int64
numWords bs
   | BSL.null bs = 0
   | otherwise = (if Word8.isSpace (BSL.index bs 0)
      then _numWordsStartsWithSpace
      else _numWordsStartsWithNonSpace) bs

leftPadUntil :: (Integral a, PrintfArg a) => Int -> a -> Builder
leftPadUntil n value = BSB.string7 $ printf (concat ["%", show n, "d"]) value

-- Get the output flags (lines, words, bytes) from the given input
-- flags

_outOptions :: [Option] -> [OutputOption]
_outOptions [] = []
-- List everything explicitly so that we get a compile error when we
-- add a new Option type.
-- http://www.haskell.org/haskellwiki/Scrap_your_boilerplate may offer
-- a better approach
_outOptions (x : xs) =
   (case x of
      Bytes -> (:) OutputBytes
      Lines -> (:) OutputLines
      Words -> (:) OutputWords
      Help -> id) $ _outOptions xs

outOptions :: [Option] -> [OutputOption]
outOptions os = let oos = _outOptions os
   in sort $ if null oos then [OutputBytes, OutputLines, OutputWords] else oos

singleFlagInteract :: OutputOption -> BSL.ByteString -> Builder
singleFlagInteract option inputStr =
   BSB.string7 $ show (_optionFunctionMap option inputStr)

multiFlagInteract :: [OutputOption] -> BSL.ByteString -> Builder
multiFlagInteract [] _ = BSB.string7 ""
multiFlagInteract (o : os) inputStr =
   leftPadUntil 7 (_optionFunctionMap o inputStr)
      `mappend` multiFlagInteract os inputStr

interactFunction :: [Option] -> BSL.ByteString -> BSL.ByteString
interactFunction opts inputStr = let outOpts = outOptions opts
   in BSB.toLazyByteString $ appendEol (
      if 1 == length outOpts
         then singleFlagInteract (head outOpts) inputStr
         else multiFlagInteract outOpts inputStr)

main :: IO ()
main = do
   argv <- getArgs
   let (opts, _args, errs) = GetOpt.getOpt GetOpt.RequireOrder _optDescr argv
      in if not $ null errs
         then ioError (userError
            (concat errs ++ GetOpt.usageInfo "" _optDescr))
         else
            if Help `elem` opts
               then
                  hPutStrLn stderr (GetOpt.usageInfo "" _optDescr)
               else
                  BSL.interact $ interactFunction opts
