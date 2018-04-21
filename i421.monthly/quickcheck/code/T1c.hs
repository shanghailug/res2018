module T1c where

import T1
import T1a(ε)

import Test.QuickCheck
import Data.Complex

-- r0 + r1 = -b, r0 * r1 = c
propAddC :: Complex Double -> Complex Double -> Bool
propAddC b c =
   let (r0, r1) = findRoots b c
   in magnitude (r0 + r1 + b) < ε

propMulC :: Complex Double -> Complex Double -> Bool
propMulC b c =
   let (r0, r1) = findRoots b c
   in magnitude (r0 * r1 - c) < ε
