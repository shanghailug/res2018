module T1a where

import T1
import Test.QuickCheck

ε :: Double
ε = 1.0e-5

propAdd :: Double -> Double -> Bool
propAdd b c =
   let (r0, r1) = findRoots b c
   in abs (r0 + r1 + b) < ε

propMul :: Double -> Double -> Bool
propMul b c =
   let (r0, r1) = findRoots b c
   in abs (r0 * r1 - c) < ε
