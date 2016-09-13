-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>
-- Stability:  experimental
--
-- Usage from GHCi:
--
-- > :seti -XOverloadedStrings
-- > hangmanStdio_ "hello"

{-# LANGUAGE FlexibleInstances #-}

module Hangman
    ( -- * Hangman game
      Hangman,
      char,
      hangmanStdio,
      hangmanStdio_,
      singleton,
    )
    where

import Data.Char
import Data.Monoid
import Data.String
import System.IO


data Hangman a b
    = Guess [Maybe b] (a -> Hangman a b)
    | Won [b]

instance IsString (Hangman Char Char) where
    fromString = foldMap char

instance Monoid (Hangman a b) where
    mappend (Guess xs f) (Guess ys g) = Guess (xs <> ys) (f <> g)
    mappend (Guess xs f) (Won ys)     = Guess (xs <> fmap Just ys) (f <> const (Won ys))
    mappend (Won xs)     (Guess ys g) = Guess (fmap Just xs <> ys) (const (Won xs) <> g)
    mappend (Won xs)     (Won ys)     = Won (xs <> ys)

    mempty = Won mempty


char :: Char -> Hangman Char Char
char y = singleton (\x -> toLower x == toLower y) y


hangmanStdio :: (String -> Maybe a) -> ([Maybe b] -> String) -> Hangman a b -> IO ()
hangmanStdio fromLine toString = go
    where
    getGuess = do
        putStr "Your guess: "
        hFlush stdout
        fromLine <$> getLine >>=
            maybe (putStrLn "What?" >> getGuess)
                  pure

    go (Guess ys f) = do
        putStrLn ("Phrase: " ++ toString ys)
        getGuess >>= go . f

    go (Won ys) =
        putStrLn ("You won: " ++ toString (Just <$> ys))


hangmanStdio_ :: Hangman Char Char -> IO ()
hangmanStdio_ = hangmanStdio (foldr (const . Just) Nothing) (map (maybe '_' id))


singleton :: (a -> Bool) -> b -> Hangman a b
singleton good y =
    Guess [Nothing] $ \x ->
        if good x
          then Won [y]
          else singleton good y
