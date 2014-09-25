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
module Main(main)
   where

import Debug.Trace
import System.IO

someIoActions :: String -> [IO ()]
someIoActions str = trace ("someIoActions" ++ str)
   [hPutStrLn stderr "BEGIN", hPutStrLn stderr str, hPutStrLn stderr "END"]

doNothing :: IO ()
doNothing = return ()

sequenceIO :: [IO ()] -> IO ()
sequenceIO [] = trace "sequence []" (return ())
sequenceIO (x : xs) = trace "sequence..." (do { x ; sequenceIO xs })

main :: IO ()
main = do
   trace "doNothing" doNothing
   trace "doNothing" doNothing
   trace "sequenceIO" (sequenceIO (trace "reverse" (reverse (
            concat [someIoActions (show n) | n <- [1..10]]))))
