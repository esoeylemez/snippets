-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>

{-# LANGUAGE ScopedTypeVariables #-}

module FibNF (fib) where

import Data.Proxy
import Data.Ratio
import Data.Reflection


data NF r a = NF !a !a
    deriving (Eq, Ord)

instance (Eq a, Fractional a, Reifies r a) => Fractional (NF r a) where
    recip (NF a b) = NF (a/d) (-b/d)
        where
        r = reflect (Proxy :: Proxy r)
        d = a*a - b*b*r

    fromRational x =
        NF (fromRational x) 0

instance (Eq a, Num a, Reifies r a) => Num (NF r a) where
    NF a1 b1 + NF a2 b2 = NF (a1 + a2) (b1 + b2)
    NF a1 b1 - NF a2 b2 = NF (a1 - a2) (b1 - b2)
    NF a1 b1 * NF a2 b2 = NF (a1*a2 + b1*b2*r) (a1*b2 + a2*b1)
        where r = reflect (Proxy :: Proxy r)

    abs = id
    negate (NF a b) = NF (-a) (-b)
    signum (NF 0 0) = NF 0 0
    signum x = x

    fromInteger ix = NF (fromInteger ix) 0

instance (Eq a, Num a, Show a, Reifies r a) => Show (NF r a) where
    showsPrec d (NF a 0) = showsPrec d a
    showsPrec d (NF a b) =
        showParen (d > 6) $
            showsPrec 6 a .
            showString " + " .
            showsPrec 7 b .
            showString "*sqrt " .
            showsPrec 10 r
        where
        r = reflect (Proxy :: Proxy r)


sqrtR :: (Num a) => NF r a
sqrtR = NF 0 1


fib :: Integer -> Integer
fib n =
    reify 5 $ \(_ :: Proxy r) ->
        let x@(NF a b) = (phi^n - (1 - phi)^n) / sqrtR :: NF r Rational
            phi = (1 + sqrtR) / 2
        in if b /= 0 || denominator a /= 1
             then error ("fib: Arithmetic error: " ++ show x)
             else numerator a
