-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>
-- Stability:  experimental

{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Data.Bits
import qualified Data.ByteString as B
import Data.Foldable
import System.Clock
import System.Environment
import Text.Printf


longestPalindrome :: B.ByteString -> B.ByteString
longestPalindrome s0 | B.null s0 = mempty
longestPalindrome s0 =
    foldl' (\s' s -> if B.length s > B.length s' then s else s') mempty .
    map palindrome $
    tail (B.inits s0) ++ init (B.tails (B.tail s0))


palindrome :: B.ByteString -> B.ByteString
palindrome bs
    | odd n     = go (m - 1) (m + 1)
    | otherwise = go (m - 1) m
    where
    n = B.length bs
    m = shiftR n 1

    go i _ | i < 0 = bs
    go i j
        | B.index bs i == B.index bs j = go (i - 1) (j + 1)
        | otherwise                    = B.drop (i + 1) (B.take j bs)


main :: IO ()
main = getArgs >>= mapM_ mainWith
    where
    mainWith fp = do
        bs' <- B.readFile fp
        t0 <- getTime Monotonic
        let bs = longestPalindrome bs'
        t1 <- bs `seq` getTime Monotonic
        putStr "Longest palindrome: |"
        B.putStr bs
        printf "| Time: %12.10f\n" (fromInteger (toNanoSecs (t1 - t0)) / 10e9 :: Double)
        printf "String length: %d\n" (B.length bs')
        printf "Palindrome length: %d\n" (B.length bs)
