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
import Data.IORef
-- import System.Mem.Weak 

data V :: (* -> *)

-- | Values that can be moved to another region via RVal
class ReProtect (s :: (* -> *) -> *) where
  unsafeChangeRegion :: s st -> s gt 
  unsafeToRefCountedFinalizer :: s r -> RefCountedFinalizer

data RVar (s :: (* -> *) -> *)  where
  RVal :: (ReProtect s) => MVar (RefCountedFinalizer, s V) -> RVar s

newRVar :: (MonadBase IO parent, region ~ RegionT st parent, ReProtect s)
        => region (RVar s)
newRVar = liftBase $ do
  m <- newEmptyMVar
  return $ RVal m

putRVar :: (MonadBase IO parent, region ~ RegionT st parent, ReProtect s)
        => RVar s -> s region -> region ()
putRVar (RVal m) s = liftBase $ mask_ $ do
   let rf@(RefCountedFinalizer _ cnt)  = unsafeToRefCountedFinalizer s
   rf `seq` putMVar m (rf, unsafeChangeRegion s)
   atomicModifyIORef' cnt (\refCnt ->
     let refCnt' = refCnt + 1
     in (refCnt', ()))

takeRVar :: (Dup s, MonadBase IO parent, RegionBaseControl IO region, region ~ RegionT st parent, ReProtect s)
         => RVar s -> region (s region)
takeRVar (RVal m) = 
   runRegionT $ unsafeControl $ \runInIO -> mask_ $ do
     (RefCountedFinalizer _ cnt, s) <- takeMVar m
     atomicModifyIORef' cnt (\refCnt ->
       let refCnt' = refCnt - 1
       in (refCnt', ()))
     runInIO $ dup (unsafeChangeRegion s)
