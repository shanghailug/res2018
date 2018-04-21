import T1
import T1a

import Test.QuickCheck

propAdd' :: Double -> Double -> Property
propAdd' b c =
   b * b >= 4 * c ==> propAdd b c

propMul' :: Double -> Double -> Property
propMul' b c =
   b * b >= 4 * c ==> propMul b c

