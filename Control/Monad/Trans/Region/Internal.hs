{-# LANGUAGE CPP                        -- For portability.
           , NoImplicitPrelude          -- I like to be fully explicit.
           , GeneralizedNewtypeDeriving -- I'm lazy.
           , RankNTypes                 -- Provides the essential type-safety.
           , KindSignatures             -- To help the type-checker.
           , EmptyDataDecls             -- For the RootRegion type.
           , MultiParamTypeClasses      -- For the AncestorRegion class.
           , UndecidableInstances       -- For the AncestorRegion instances.
           , FlexibleInstances          -- ,,          ,,          ,,
           , OverlappingInstances       -- ,,          ,,          ,,
           , TypeFamilies               -- For the RegionBaseControl class.
           , FunctionalDependencies     -- ,,          ,,          ,,
           , FlexibleContexts           -- For MonadBase.
  #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Region.Internal
-- Copyright   :  (c) 2009-2011 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This modules implements a technique called /"Lightweight monadic regions"/
-- invented by Oleg Kiselyov and Chung-chieh Shan
--
-- See: <http://okmij.org/ftp/Haskell/regions.html#light-weight>
--
-- This is an internal module not intended to be exported.
-- Use @Control.Monad.Trans.Region@  or @Control.Monad.Trans.Region.Unsafe@.
--
--------------------------------------------------------------------------------

module Control.Monad.Trans.Region.Internal
    ( -- * Regions
      RegionT

      -- * Registering finalizers
    , Finalizer
    , FinalizerHandle
    , onExit

      -- * Running regions
    , runRegionT

      -- * Duplication
    , Dup(dup)

      -- * Ancestor relation between regions
    , AncestorRegion

      -- * Special regions
      -- ** The root region
    , RootRegion

      -- ** Local regions
    , LocalRegion, Local, unsafeStripLocal

      -- * RegionBaseControl & MonadBaseControl
    , RegionBaseControl
    , unsafeLiftBaseWith
    , unsafeControl
    , unsafeLiftBaseOp
    , unsafeLiftBaseOp_

      -- * Utilities for writing monadic instances
    , liftCallCC
    , mapRegionT
    , liftCatch
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Prelude             ( (+), (-), seq )
import Control.Applicative ( Applicative, Alternative )
import Control.Monad       ( Monad, return, when, forM_, liftM, MonadPlus, (>>=) )
import Control.Monad.Fix   ( MonadFix )
import Control.Exception   ( bracket )
import System.IO           ( IO )
import Data.Function       ( ($), (.) )
import Data.Functor        ( Functor )
import Data.Eq             ( (==) )
import Data.Int            ( Int )
import Data.IORef          ( IORef, newIORef
                           , readIORef, modifyIORef, atomicModifyIORef
                           )
#if __GLASGOW_HASKELL__ < 700
import Prelude             ( fromInteger )
import Control.Monad       ( (>>=), (>>), fail )
#endif

-- from monad-control:
import Control.Monad.Trans.Control (MonadTransControl(..), MonadBaseControl(..))

-- from transformers-base:
import Control.Monad.Base (MonadBase, liftBase, liftBaseDefault)

-- from transformers:
import Control.Monad.Trans.Class ( MonadTrans, lift )
import Control.Monad.IO.Class    ( MonadIO )

import qualified Control.Monad.Trans.Reader as R ( liftCallCC, liftCatch )
import Control.Monad.Trans.Reader ( ReaderT(ReaderT), runReaderT, mapReaderT )

-- Handling the new asynchronous exceptions API in base-4.3:
#if MIN_VERSION_base(4,3,0)
import Control.Exception ( mask_ )
#else
import Control.Exception ( block )
mask_ :: IO a -> IO a
mask_ = block
#endif


--------------------------------------------------------------------------------
-- * Regions
--------------------------------------------------------------------------------

{-| A monad transformer in which scarce resources can be opened. When the region
terminates, all opened resources will be closed automatically. It's a type error
to return an opened resource from the region. The latter ensures no I/O with
closed resources is possible.
-}
newtype RegionT s pr a = RegionT (RegionStT pr a)
    deriving ( Functor
             , Applicative
             , Alternative
             , Monad
             , MonadPlus
             , MonadFix
             , MonadTrans
             , MonadIO
             )

-- | The state of a region is a list of resource finalizers which need to be
-- called when the region terminates. The state is represented using an 'IORef'
-- and the passing around of this 'IORef' is abstracted using a 'ReaderT' monad
-- transformer.
--
-- Note that finalizers are reference counted. At the end of running a region
-- all counts are decremented. When the count reaches 0 the finalizer is
-- executed. When a finalizer needs to be copied to a parent region using 'dup'
-- the count is incremented.
type RegionStT = ReaderT (IORef [RefCountedFinalizer])

unRegionT :: RegionT s pr a -> RegionStT pr a
unRegionT (RegionT r) = r

-- | A 'Finalizer' paired with its reference count which defines how many times
-- it has been registered in some region.
data RefCountedFinalizer = RefCountedFinalizer !Finalizer !(IORef RefCnt)

-- | An 'IO' computation that closes or finalizes a resource. For example
-- \"@hClose someHandle@\" or \"@free somePtr@\".
type Finalizer = IO ()

type RefCnt = Int


--------------------------------------------------------------------------------
-- * Registering finalizers
--------------------------------------------------------------------------------

{-| A handle to a 'Finalizer' that allows you to duplicate it to a parent region
using 'dup'.

Duplicating a finalizer means that instead of it being performed when the
current region terminates it is performed when the parent region terminates.
-}
newtype FinalizerHandle (r :: * -> *) = FinalizerHandle RefCountedFinalizer

{-| Register the 'Finalizer' in the region. When the region terminates all
registered finalizers will be perfomed if they're not duplicated to a parent
region.

Note that finalizers are run in LIFO order (Last In First Out). So executing the following:

@
runRegionT $ do
  _ <- onExit $ putStrLn \"finalizer 1\"
  _ <- onExit $ putStrLn \"finalizer 2\"
  return ()
@

yields:

@
finalizer 2
finalizer 1
@
-}
onExit :: (MonadBase IO pr) => Finalizer -> RegionT s pr (FinalizerHandle (RegionT s pr))
onExit finalizer = RegionT $ ReaderT $ \hsIORef -> liftBase $ do
                     refCntIORef <- newIORef 1
                     let h = RefCountedFinalizer finalizer refCntIORef
                     modifyIORef hsIORef (h:)
                     return $ FinalizerHandle h


--------------------------------------------------------------------------------
-- * Running regions
--------------------------------------------------------------------------------

{-| Execute a region inside its parent region @pr@.

All resources which have been opened in the given region and which haven't been
duplicated using 'dup', will be closed on exit from this function wether by
normal termination or by raising an exception.

Also all resources which have been duplicated to this region from a child region
are closed on exit if they haven't been duplicated themselves.

Note the type variable @s@ of the region wich is only quantified over the region
itself. This ensures that /all/ values, having a type containing @s@, can /not/
be returned from this function. (Note the similarity with the @ST@ monad.)

Note that it is possible to run a region inside another region.
-}
runRegionT :: RegionBaseControl IO pr => (forall s. RegionT s pr a) -> pr a
runRegionT r = unsafeLiftBaseOp (bracket before after) thing
    where
      before = newIORef []
      thing hsIORef = runReaderT (unRegionT r) hsIORef
      after hsIORef = do
        hs' <- readIORef hsIORef
        forM_ hs' $ \(RefCountedFinalizer finalizer refCntIORef) -> do
          refCnt <- decrement refCntIORef
          when (refCnt == 0) finalizer
          where
            decrement :: IORef RefCnt -> IO RefCnt
            decrement ioRef = atomicModifyIORef ioRef $ \refCnt ->
                                let refCnt' = refCnt - 1
                                in (refCnt', refCnt')


--------------------------------------------------------------------------------
-- * Duplication
--------------------------------------------------------------------------------

{-| Duplicate an @h@ in the parent region. This @h@ will usually be some type of
regional handle.

For example, suppose you run the following region:

@
runRegionT $ do
@

Inside this region you run a nested /child/ region like:

@
    r1hDup <- runRegionT $ do
@

Now in this child region you open the resource @r1@:

@
        r1h <- open r1
@

...yielding the regional handle @r1h@. Note that:

@r1h :: RegionalHandle (RegionT cs (RegionT ps ppr))@

where @cs@ is bound by the inner (child) @runRegionT@ and @ps@ is
bound by the outer (parent) @runRegionT@.

Suppose you want to use the resulting regional handle @r1h@ in the /parent/
region. You can't simply @return r1h@ because then the type variable @cs@,
escapes the inner region.

However, if you duplicate the regional handle you can safely return it.

@
        r1hDup <- dup r1h
        return r1hDup
@

Note that @r1hDup :: RegionalHandle (RegionT ps ppr)@

Back in the parent region you can safely operate on @r1hDup@.
-}
class Dup h where
    dup :: (MonadBase IO ppr)
        => h (RegionT cs (RegionT ps ppr))
        ->    RegionT cs (RegionT ps ppr)
                     (h (RegionT ps ppr))

instance Dup FinalizerHandle where
    dup = lift . copy

copy :: MonadBase IO pr
     => FinalizerHandle r
     -> RegionT s pr (FinalizerHandle (RegionT s pr))
copy (FinalizerHandle h@(RefCountedFinalizer _ refCntIORef)) =
  RegionT $ ReaderT $ \hsIORef -> liftBase $ mask_ $ do
    increment refCntIORef
    modifyIORef hsIORef (h:)
    return $ FinalizerHandle h

increment :: IORef RefCnt -> IO ()
increment ioRef = do refCnt' <- atomicModifyIORef ioRef $ \refCnt ->
                                  let refCnt' = refCnt + 1
                                  in (refCnt', refCnt')
                     refCnt' `seq` return ()


--------------------------------------------------------------------------------
-- * Ancestor relation between regions
--------------------------------------------------------------------------------

{-| The @AncestorRegion@ class is used to relate the region in which a resource
was opened to the region in which it is used. Take the following operation from
the @safer-file-handles@ package as an example:

@hFileSize :: (pr \`AncestorRegion\` cr, MonadIO cr) => RegionalFileHandle ioMode pr -> cr Integer@

The @AncestorRegion@ class defines the parent / child relationship between regions.
The constraint

@
    pr \`AncestorRegion\` cr
@

is satisfied if and only if @cr@ is a sequence of zero or more \"@'RegionT' s@\"
(with varying @s@) applied to @pr@, in other words, if @cr@ is an (improper)
nested subregion of @pr@.

The class constraint @InternalAncestorRegion pr cr@ serves two purposes. First, the
instances of @InternalAncestorRegion@ do the type-level recursion that implements
the relation specified above. Second, since it is not exported, user code cannot
add new instances of 'AncestorRegion' (as these would have to be made instances of
@InternalAncestorRegion@, too), effectively turning it into a /closed class/.
-}

-- The implementation uses type-level recursion, so it is no surprise we need
-- UndecidableInstances.

class (InternalAncestorRegion pr cr) => AncestorRegion pr cr

instance (InternalAncestorRegion pr cr) => AncestorRegion pr cr

class InternalAncestorRegion (pr :: * -> *) (cr :: * -> *)

instance InternalAncestorRegion (RegionT s m) (RegionT s m)
instance (InternalAncestorRegion pr cr) => InternalAncestorRegion pr (RegionT s cr)


--------------------------------------------------------------------------------
-- * The root region
--------------------------------------------------------------------------------

{-| The @RootRegion@ is the ancestor of any region.

It's primary purpose is to tag regional handles which don't have an associated
finalizer. For example the standard file handles @stdin@, @stdout@ and @stderr@
which are opened on program startup and which shouldn't be closed when a region
terminates. Another example is the @nullPtr@ which is a memory pointer which
doesn't point to any allocated memory so doesn't need to be freed.
-}
data RootRegion a

instance InternalAncestorRegion RootRegion (RegionT s m)


--------------------------------------------------------------------------------
-- * Local regions
--------------------------------------------------------------------------------

{-|
A @LocalRegion@ is used to tag regional handles which are created locally.

An example is the @LocalPtr@ in the @alloca@ function from the
@regional-pointers@ package:

@
alloca :: (Storable a, RegionBaseControl IO pr)
       => (forall sl. LocalPtr a ('LocalRegion' sl s) -> RegionT ('Local' s) pr b)
       -> RegionT s pr b
@

The finalisation of the @LocalPtr@ is not performed by the @regions@ library but
is handled locally by @alloca@ instead.

The type variable @sl@, which is only quantified over the continuation, ensures
that locally opened resources don't escape.
-}
data LocalRegion sl s a

{-|
A type used to tag regions in which locally created handles (handles tagged with
'LocalRegion') can be used.

Note than any handle which is created in a @RegionT (Local s)@ can be used
outside that region (@RegionT s@) and visa versa
(except for 'LocalRegion'-tagged handles).
-}
data Local s

instance InternalAncestorRegion (LocalRegion sf s) (RegionT (Local s) m)

instance InternalAncestorRegion (RegionT        s  m) (RegionT (Local s) m)
instance InternalAncestorRegion (RegionT (Local s) m) (RegionT        s  m)

{-|
Convert a 'Local' region to a regular region.

This function is unsafe because it allows you to use a 'LocalRegion'-tagged
handle outside its 'Local' region.
-}
unsafeStripLocal :: RegionT (Local s) pr a -> RegionT s pr a
unsafeStripLocal = RegionT . unRegionT


--------------------------------------------------------------------------------
-- * MonadTransControl & MonadControlIO
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- ** MonadBaseControl type class
--------------------------------------------------------------------------------

{-|
Regions do not have an instance for 'MonadBaseControl' since that would break the
safety guarantees. (Think about lifting 'forkIO' into a region!)

However 'runRegionT' and other operations on regions do need the ability to lift
control operations. This is where the 'RegionBaseControl' class comes in. This
class is identical to `MonadBaseControl` but its 'unsafeLiftBaseWith' method is
not exported by this module. So users can't accidentally break the safety.

Note that a 'RegionT' is an instance of this class. For the rest there is a
catch-all @instance 'MonadBaseControl' m => 'RegionControlControl' m@.
-}
class MonadBase b m => RegionBaseControl b m | m -> b where
    -- | Monadic state of @m@.
    data RegionStM m :: * -> *

    -- | @liftBaseWith@ is similar to 'liftIO' and 'liftBase' in that it
    -- lifts a base computation to the constructed monad.
    --
    -- Instances should satisfy similar laws as the 'MonadIO' and 'MonadBase' laws:
    --
    -- @liftBaseWith . const . return = return@
    --
    -- @liftBaseWith (const (m >>= f)) = liftBaseWith (const m) >>= liftBaseWith . const . f@
    --
    -- The difference with 'liftBase' is that before lifting the base computation
    -- @liftBaseWith@ captures the state of @m@. It then provides the base
    -- computation with a 'RunInBase' function that allows running @m@
    -- computations in the base monad on the captured state.
    unsafeLiftBaseWith :: (RunRegionInBase m b -> b a) -> m a

    -- | Construct a @m@ computation from the monadic state of @m@ that is
    -- returned from a 'RunInBase' function.
    --
    -- Instances should satisfy:
    --
    -- @liftBaseWith (\\runInBase -> runInBase m) >>= restoreM = m@
    unsafeRestoreM :: RegionStM m a -> m a

-- | A function that runs a @m@ computation on the monadic state that was
-- captured by 'liftBaseWith'
--
-- A @RunInBase m@ function yields a computation in the base monad of @m@ that
-- returns the monadic state of @m@. This state can later be used to restore the
-- @m@ computation using 'restoreM'.
type RunRegionInBase m b = forall a. m a -> b (RegionStM m a)

instance (RegionBaseControl b pr) => RegionBaseControl b (RegionT s pr) where
    newtype RegionStM (RegionT s pr) a =
            RegionStR {unRegionStR :: RegionStM pr (StT RegionStT a)}

    unsafeLiftBaseWith f =
        RegionT $
          liftWith $ \runRdr ->
            unsafeLiftBaseWith $ \runInBase ->
              f $ liftM RegionStR . runInBase . runRdr . unRegionT

    unsafeRestoreM = RegionT . restoreT . unsafeRestoreM . unRegionStR

instance (MonadBaseControl b m) => RegionBaseControl b m where
    newtype RegionStM m a = RegionStM {unRegionStM :: StM m a}

    unsafeLiftBaseWith f =
      liftBaseWith $ \runM -> f (liftM RegionStM . runM)

    unsafeRestoreM = restoreM . unRegionStM

instance (MonadBase b pr) => MonadBase b (RegionT s pr) where
    liftBase = liftBaseDefault

-- | An often used composition:
-- @unsafeControl f = 'unsafeLiftBaseWith' f >>= 'unsafeRestoreM'@
unsafeControl :: (RegionBaseControl b m)
              => (RunRegionInBase m b -> b (RegionStM m a)) -> m a
unsafeControl f = unsafeLiftBaseWith f >>= unsafeRestoreM
{-# INLINE unsafeControl #-}

unsafeLiftBaseOp :: RegionBaseControl b m
           => ((a -> b (RegionStM m c)) -> b (RegionStM m d))
           -> ((a ->              m c)  ->              m d)
unsafeLiftBaseOp f = \g -> unsafeControl $ \runInBase -> f $ runInBase . g
{-# INLINE unsafeLiftBaseOp #-}

unsafeLiftBaseOp_ :: RegionBaseControl b m
            => (b (RegionStM m a) -> b (RegionStM m c))
            -> (             m a  ->              m c)
unsafeLiftBaseOp_ f = \m -> unsafeControl $ \runInBase -> f $ runInBase m
{-# INLINE unsafeLiftBaseOp_ #-}

--------------------------------------------------------------------------------
-- * Utilities for writing monadic instances
--------------------------------------------------------------------------------

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: (((a -> pr b) -> pr a) -> pr a)
           -> (((a -> RegionT s pr b) -> RegionT s pr a) -> RegionT s pr a)
liftCallCC callCC = \f -> RegionT $ R.liftCallCC callCC $ unRegionT . f . (RegionT .)

-- | Transform the computation inside a region.
mapRegionT :: (m a -> n b)
           -> (RegionT s m a -> RegionT s n b)
mapRegionT f = RegionT . mapReaderT f . unRegionT

-- | Lift a @catchError@ operation to the new monad.
liftCatch :: (pr a -> (e -> pr a) -> pr a)
          -> (RegionT s pr a -> (e -> RegionT s pr a) -> RegionT s pr a)
liftCatch f = \m h -> RegionT $ R.liftCatch f (unRegionT m) (unRegionT . h)
