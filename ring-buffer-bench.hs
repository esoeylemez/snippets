{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Control.Monad.Primitive
import Data.Primitive.MutVar
import Data.Proxy
import Data.Sequence (ViewL(..), (|>), viewl)
import qualified Data.Sequence as Seq
import qualified Data.Vector.Generic.Mutable as Vm
import qualified Data.Vector.Mutable as Vbm
import qualified Data.Vector.Unboxed.Mutable as Vum
import System.Environment
import System.Random.MWC
import System.Random.MWC.Distributions


-- Ring buffer version -----------------------------------------------

data RingBuffer s v a =
    RingBuffer {
      _rbLeft  :: !(MutVar s Int),
      _rbRight :: !(MutVar s Int),
      _rbVec   :: v s a
    }

type RingBufferIO = RingBuffer (PrimState IO)


rbNew :: (PrimMonad m, Vm.MVector v a) => Int -> m (RingBuffer (PrimState m) v a)
rbNew n = do
    _rbLeft <- newMutVar (-1)
    _rbRight <- newMutVar 0
    _rbVec <- Vm.new n
    pure RingBuffer{..}


rbPop :: (PrimMonad m, Vm.MVector v a) => RingBuffer (PrimState m) v a -> m (Maybe a)
rbPop RingBuffer{..} = do
    let len = Vm.length _rbVec
    i''' <- readMutVar _rbLeft
    if i''' == -1
      then pure Nothing
      else do
          j <- readMutVar _rbRight

          let i'' = i''' + 1
              i' | i'' >= len = 0
                 | otherwise  = i''

              i | i' == j   = -1
                | otherwise = i'

          writeMutVar _rbLeft i
          Just <$> Vm.read _rbVec i'''


rbPush :: (PrimMonad m, Vm.MVector v a) => RingBuffer (PrimState m) v a -> a -> m ()
rbPush RingBuffer{..} x = do
    let len = Vm.length _rbVec
    i' <- readMutVar _rbLeft
    j'' <- readMutVar _rbRight

    let i | i' == -1  = j''
          | i' == j'' = j
          | otherwise = i'

        j' = j'' + 1
        j | j' >= len = 0
          | otherwise = j'

    writeMutVar _rbLeft $! i
    writeMutVar _rbRight $! j
    Vm.write _rbVec j'' x


rbBench :: forall m proxy v. (PrimMonad m, Vm.MVector v Int) => proxy v -> Gen (PrimState m) -> Int -> Int -> m ()
rbBench _ rng size n = do
    rb <- rbNew size :: m (RingBuffer (PrimState m) v Int)
    replicateM_ n $ do
        wantPush <- bernoulli 0.7 rng
        if wantPush
          then do x <- uniform rng; rbPush rb x
          else rbPop rb >>= \mx -> maybe id seq mx (pure ())


-- Seq version -------------------------------------------------------

seqBench :: (PrimMonad m) => Gen (PrimState m) -> Int -> Int -> m ()
seqBench rng size = go mempty
    where
    go _ 0 = pure ()
    go xs'' n = do
        wantPush <- bernoulli 0.7 rng
        if wantPush
          then do
              x :: Int <- uniform rng
              let xs | Seq.length xs'' >= size =
                         case viewl xs'' of
                           EmptyL   -> xs''
                           _ :< xs' -> xs' |> x
                     | otherwise = xs'' |> x
              xs `seq` go xs (n - 1)
          else
              case viewl xs'' of
                EmptyL  -> go xs'' (n - 1)
                _ :< xs -> go xs (n - 1)


main :: IO ()
main = do
    sizeStr : nStr : variant : _ <- getArgs
    let n = read nStr
        s = read sizeStr
    rng <- create
    case variant of
      "s"  -> seqBench rng s n
      "vb" -> rbBench (Proxy :: Proxy Vbm.MVector) rng s n
      "vu" -> rbBench (Proxy :: Proxy Vum.MVector) rng s n
      _    -> pure ()
