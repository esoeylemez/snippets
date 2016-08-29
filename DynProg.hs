-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>

module DynProg
    ( -- * Dynamic programming
      fibUpTo
    )
    where

import qualified Data.Vector as V


-- | Dynamic programming version of the Fibonacci function.  Note that
-- for any maximum index @n@ all the local values are attached to the
-- function @fibUpTo n@, so in the following snippets they will be
-- shared:
--
-- >>> let fib = fibUpTo 20 in fib 18 + fib 19
-- 6765
--
-- >>> map (fibUpTo 20) [0..19]
-- [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181]
--
-- However, in the following snippet they are unlikely to be shared,
-- because the function itself is not shared:
--
-- >>> fibUpTo 20 18 + fibUpTo 20 19
-- 6765

fibUpTo :: Int -> Int -> Integer
fibUpTo n = fib
    where
    fib = (vec V.!)
    vec = V.generate n f

    f 0 = 0
    f 1 = 1
    f i = fib (i - 1) + fib (i - 2)
