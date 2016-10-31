-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PackageImports #-}

module Main (main) where

import Control.Monad.ST
import Data.Foldable
import Data.Int
import Data.List (unfoldr)
import Data.Tuple
import qualified "vector-algorithms" Data.Vector.Algorithms.Search as Vas
import qualified "vector" Data.Vector.Unboxed as Vu
import qualified "vector" Data.Vector.Unboxed.Mutable as Vum


chainLength :: (Integral a, Vu.Unbox a) => a -> Int
chainLength =
    \x0 -> runST $ do
        arr <- Vum.new 61
        let go i x = do
                j <- Vas.binarySearch (Vum.unsafeTake i arr) (fromIntegral x :: Int32)
                let ins = do
                        let l = i - j
                        Vum.unsafeMove (Vum.slice (j + 1) l arr) (Vum.slice j l arr)
                        Vum.unsafeWrite arr j (fromIntegral x)
                if j >= i
                  then ins >> go (i + 1) (digitFact x)
                  else do
                      y <- Vum.unsafeRead arr j
                      if x == fromIntegral y
                        then pure i
                        else ins >> go (i + 1) (digitFact x)
        go 0 x0

    where
    !factTable = Vu.scanl' (*) 1 (Vu.enumFromTo 1 9)

    digitFact =
        foldl' (+) 0 .
        map (\i -> factTable Vu.! fromIntegral i) .
        unfoldr (\x -> if x == 0 then Nothing else Just (swap (divMod x 10)))

{-# INLINE chainLength #-}


main :: IO ()
main =
    print .
    length .
    filter (== 60) .
    map chainLength $
    [1..999999 :: Int]
