
-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Region
-- Copyright   :  (c) 2009 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This modules implements a technique called /"Lightweight monadic regions"/
-- invented by Oleg Kiselyov and Chung-chieh Shan
--
-- See: <http://okmij.org/ftp/Haskell/regions.html#light-weight>
--
--------------------------------------------------------------------------------

module Control.Monad.Trans.Region
    ( -- * Regions
      RegionT

      -- * Running regions
    , runRegionT

    , TopRegion
    , runTopRegion
    , forkTopRegion

      -- * Opening resources
    , RegionalHandle
    , open

    , with

      -- * Duplication
    , Dup
    , dup

      -- * Parent/child relationship between regions.
    , ParentOf

      -- * Handy functions for writing monadic instances
    , mapRegionT
    , liftCatch
      -- | /TODO: define and export: /@liftCallCC@
    ) where

import Control.Monad.Trans.Region.Internal
