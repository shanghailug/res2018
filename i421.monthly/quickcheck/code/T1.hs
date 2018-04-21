module T1 where

findRoots :: Floating a => a -> a -> (a, a)
findRoots b c =
  let a = 1
      δ = b * b - 4 * a * c
  in ( (- b - sqrt δ) / 2, (-b + sqrt δ) / 2)
