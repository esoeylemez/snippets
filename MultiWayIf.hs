-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>
-- Stability:  experimental

module MultiWayIf
    ( -- * Functional multi-way if
      (-->),
      (==>),
      (?)
    )
    where


(-->) :: Bool -> a -> a -> a
(-->) True = const
(-->) False = const id

infix 1 -->


(==>) :: Maybe b -> (b -> a) -> a -> a
mx ==> f = maybe id (const . f) mx


(?) :: (a -> a) -> a -> a
(?) = id

infixr 0 ?


_example :: String
_example =
    even 3 --> "3 is even" ?
    even 5 --> "5 is even" ?
    even 6 --> "6 is even" ?
    "none of them are even"
