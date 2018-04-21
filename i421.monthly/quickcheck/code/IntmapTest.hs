{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array
import Control.Monad

import Data.IORef
import Test.QuickCheck
import Test.QuickCheck.Monadic

import qualified Data.IntMap as IM
import Data.IntMap(IntMap)

foreign import ccall safe "intmap_test.h im_init" imInit :: IO ()
foreign import ccall safe "intmap_test.h im_lookup" im_lookup :: CInt -> IO CInt
foreign import ccall safe "intmap_test.h im_put" im_put :: CInt -> CInt -> IO CInt
foreign import ccall safe "intmap_test.h im_delete" im_delete :: CInt -> IO CInt

ci :: Functor a => a CInt -> a Int
ci = fmap fromIntegral

-- return lookup key or 0
imLookup :: Int -> IO Int
imLookup key = ci $ im_lookup $ fromIntegral key

-- return replaced value, or 0 is this key not used
imPut :: Int -> Int -> IO Int
imPut key idx = ci $ im_put (fromIntegral key) (fromIntegral idx)

-- return 0, not exist
imDelete :: Int -> IO Int
imDelete key = ci $ im_delete $ fromIntegral key

{-
op: lookup, put & delete
check by lookup every element in model
-}

data IntmapOp = OpLookup
              | OpPut
              | OpDelete
              deriving (Eq, Show)

instance Arbitrary IntmapOp where
  arbitrary = frequency [
    (10, return OpLookup),
    (70, return OpPut),
    (20, return OpDelete)
    ]

prop_t1 :: [IntmapOp] -> Property
prop_t1 ops = monadicIO $ do
  run $ imInit
  ref <- run $ newIORef IM.empty
  forM_ ops $ \op -> doOp ref op >> doCheck ref

doCheck :: IORef (IntMap Int) -> PropertyM IO ()
doCheck ref = do
  m <- run $ readIORef ref
  forM_ (IM.toList m) $ \(k, v') ->
    do v <- run $ imLookup k
       assert $ v == v'

doOp :: IORef (IntMap Int) -> IntmapOp -> PropertyM IO ()

doOp ref OpLookup = do
  m <- run $ readIORef ref
  k <- pick arbitrary
  v <- run $ imLookup k
  case IM.lookup k m of
    Just v' -> assert $ v == v'
    Nothing -> assert $ v == 0

  return ()

doOp ref OpPut = do
  m <- run $ readIORef ref
  k <- pick $ oneof [arbitrary, oneof $ map return $ 0 : IM.keys m]
  v1 <- pick $ suchThat arbitrary (/= 0) -- only non zero value
  --v1 <- pick $ oneof $ map return [1 .. 10]
  v2 <- run $ imPut k v1

  --run $ print ("put", k, v1, v2)
  case IM.lookup k m of
    Just v0 ->
      do assert $ v2 == v0 -- this key already used
         monitor (label "put=")
    Nothing ->
      do assert $ v2 == 0
         monitor (label "put+")
  -- update
  run $ writeIORef ref $ IM.insert k v1 m


doOp ref OpDelete = do
  m <- run $ readIORef ref
  k <- pick $ oneof [arbitrary, oneof $ map return $ 0 : IM.keys m]
  v <- run $ imDelete k
  --run $ print ("del", k, v)
  case IM.lookup k m of
    Just v' -> do assert $ v == v' -- key exist
                  run $ writeIORef ref $ IM.delete k m
                  monitor (label "del+")
    Nothing -> do assert $ v == 0 -- key not exist
                  monitor $ label "del="

main = do
  quickCheckWith stdArgs { maxSuccess = 10000, maxSize = 500 } prop_t1
