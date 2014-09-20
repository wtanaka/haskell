#!/usr/bin/env runhaskell
import Data.Char
import Data.List
import NinetyNine
import Test.QuickCheck
import Text.Printf

main = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

prop_plusRem_nonnegative num denom = denom /= 0
   ==> NinetyNine._plusRem num denom >= 0

prop_plusRem_lessthandenom num denom = denom /= 0
   ==> NinetyNine._plusRem num denom < abs denom

prop_rotate_reversible xs count =
   NinetyNine.rotate (NinetyNine.rotate xs count) (-count) == xs

tests = [("plusRem is nonnegative", quickCheck prop_plusRem_nonnegative)
   ,("plusRem < denom", quickCheck prop_plusRem_lessthandenom)
   ,("rotate is reversible", quickCheck
      (prop_rotate_reversible :: [Int] -> Int -> Bool))
   ]
