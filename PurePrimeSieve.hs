-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>

{-# LANGUAGE BangPatterns #-}

module PurePrimeSieve (primes) where

import qualified Data.Stream as Str
import Data.Stream (Stream(..))


primes :: Stream Int
primes = sieve (Just <$> Str.iterate succ 2)
    where
    sieve (Cons Nothing xs) = sieve xs
    sieve (Cons (Just p) xs) = Cons p (sieve begin)
        where
        !i = p - 1

        begin =
            let (pfx, xs1) = Str.splitAt (p*p - p - 1) xs
            in Str.prefix pfx (scratch xs1)

        scratch (Cons _ xs') =
            let (xs1, xs2) = Str.splitAt i xs'
            in Cons Nothing (Str.prefix xs1 (scratch xs2))
