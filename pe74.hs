-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>

{-# LANGUAGE PackageImports #-}

module Main (main) where

import Control.Monad.ST
import Data.Foldable
import Data.List (unfoldr)
import Data.Tuple
import qualified "vector-algorithms" Data.Vector.Algorithms.Search as Vas
import qualified "vector" Data.Vector.Unboxed as Vu
import qualified "vector" Data.Vector.Unboxed.Mutable as Vum


digitFact :: Int -> Int
digitFact =
    foldl' (+) 0 .
    map (factTable Vu.!) .
    unfoldr (\x -> if x == 0 then Nothing else Just (swap (divMod x 10)))


factTable :: Vu.Vector Int
factTable = Vu.scanl' (*) 1 (Vu.enumFromTo 1 9)


chainLength :: Int -> Int
chainLength x0 =
    runST $ do
        arr <- Vum.new 61
        let go i x = do
                j <- Vas.binarySearch (Vum.unsafeTake i arr) x
                let ins = do
                        let l = i - j
                        Vum.unsafeMove (Vum.slice (j + 1) l arr) (Vum.slice j l arr)
                        Vum.unsafeWrite arr j x
                if j >= i
                  then ins >> go (i + 1) (digitFact x)
                  else do
                      y <- Vum.unsafeRead arr j
                      if x == y
                        then pure i
                        else ins >> go (i + 1) (digitFact x)
        go 0 x0


main :: IO ()
main =
    print .
    length .
    filter (== 60) .
    map chainLength $
    [1..999999]
