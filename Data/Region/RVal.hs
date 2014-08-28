-------------------------------------------------------------------------------
-- |
-- Module      :  Data.Region.RVal 
-- Copyright   :  (c) 2014 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This modules implements a container for the technique called
-- /"Lightweight monadic regions"/ invented by Oleg Kiselyov and Chung-chieh Shan
--
-- See: <http://okmij.org/ftp/Haskell/regions.html#light-weight>
--
--------------------------------------------------------------------------------
{-# LANGUAGE KindSignatures    -- to help typechecker
           , FlexibleContexts  -- for MonadBase
           , GADTs             -- for constraint on datatype
           , EmptyDataDecls    -- for V
           , RecursiveDo       -- for newRVar
           #-}
module Data.Region.RVal
  ( -- * Region values
    RVar
  , newRVar
  , putRVar
  , takeRVar
  ) where

import Control.Monad.Trans.Region.Internal

-- from transformers-base:
import Control.Monad.Base (MonadBase, liftBase)

-- from base
import Control.Concurrent.MVar
import Control.Exception (mask_)
import Control.Monad (when)
import Data.IORef
import Data.Foldable
import System.Mem.Weak 

data V :: (* -> *)

-- | Values that can be moved to another region via RVal
class ReProtect (s :: (* -> *) -> *) where
  unsafeChangeRegion :: s st -> s gt 
  unsafeToRefCountedFinalizer :: s r -> RefCountedFinalizer

data RVar (s :: (* -> *) -> *)  where
  RVal :: (ReProtect s) => MVar (RefCountedFinalizer, s V) -> Weak (RVar s) -> RVar s

newRVar :: (MonadBase IO parent, region ~ RegionT st parent, ReProtect s)
        => region (RVar s)
newRVar = liftBase $ mdo
    m <- newEmptyMVar
    r <- return $ RVal m w
    w <- mkWeakPtr r (Just (cleanRVal m))
    return r

putRVar :: (MonadBase IO parent, region ~ RegionT st parent, ReProtect s)
        => RVar s -> s region -> region ()
putRVar (RVal m _) s = liftBase $ mask_ $ do
   let rf@(RefCountedFinalizer _ cnt)  = unsafeToRefCountedFinalizer s
   rf `seq` putMVar m (rf, unsafeChangeRegion s)
   atomicModifyIORef' cnt (\refCnt ->
     let refCnt' = refCnt + 1
     in (refCnt', ()))

takeRVar :: (Dup s, MonadBase IO parent, RegionBaseControl IO region, region ~ RegionT st parent, ReProtect s)
         => RVar s -> region (s region)
takeRVar (RVal m _) = 
   runRegionT $ unsafeControl $ \runInIO -> mask_ $ do
     (RefCountedFinalizer _ cnt, s) <- takeMVar m
     atomicModifyIORef' cnt (\refCnt ->
       let refCnt' = refCnt - 1
       in (refCnt', ()))
     runInIO $ dup (unsafeChangeRegion s)

cleanRVal :: MVar (RefCountedFinalizer, a) -> IO ()
cleanRVal m = mask_ $ traverse_ (go . fst) =<< (tryTakeMVar m)
  where
    go (RefCountedFinalizer finalizer refCntIORef) = do
      refCnt <- decrement refCntIORef
      when (refCnt == 0) finalizer
      where
        decrement :: IORef Int -> IO Int
        decrement ioRef = atomicModifyIORef' ioRef $ \refCnt ->
                            let refCnt' = refCnt - 1
                            in (refCnt', refCnt')
