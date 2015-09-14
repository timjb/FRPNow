{-# LANGUAGE GADTs, DataKinds, KindSignatures #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Control.FRPNow.TypesafeCore (
  N (..), LTE (..),

  Event,
  Behavior,
  never,
  lookEvent,
  switch,
  whenJust,
  futuristic,
  -- * IO interface
  Now,
  async,
  asyncOS,
  callback,
  sampleNow,
  planNow,
  sync,
  -- * Entry point
  runNowMaster,
  initNow,
  rewindTime
) where

import qualified Control.FRPNow.Core as C
import qualified Control.FRPNow.Lib as L
import Control.Applicative
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Arrow (first)
import Data.Void (Void)

import GHC.Exts (Constraint)

class Impossible where
  impossible :: Void

data N = Z | S N

type family LTE (n :: N) (m :: N) :: Constraint where
  LTE n n = ()
  LTE n (S m) = LTE n m
  LTE n Z = Impossible

{-
class LTE
instance LTE n n
instance LTE n m => LTE n (S m)
-}


newtype Event (n :: N) a = Event { getEvent :: C.Event a }
  deriving (Functor, Applicative, Monad)

never :: Event n a
never = Event C.never

newtype Behavior (n :: N) a = Behavior { getBehavior :: C.Behavior a }
  deriving (Functor, Applicative, Monad, MonadFix)

lookEvent :: Event m a -> Behavior n (Either a (Event (S n) a))
lookEvent (Event e) =
  Behavior $ C.switch (pure (Right (Event e))) (pure . Left <$> e)

switch
  :: Behavior n a -> Event n (Behavior k a) -> Behavior n a
switch (Behavior b) (Event e) = Behavior $ C.switch b (getBehavior <$> e)

whenJust :: Behavior m (Maybe a) -> Behavior n (Event (S n) a)
whenJust (Behavior b) = Behavior $ Event <$> C.whenJust b

futuristic :: Behavior n (Event (S n) a) -> Behavior n (Event (S n) a)
futuristic = Behavior . fmap Event . C.futuristic . fmap getEvent . getBehavior

newtype Now (n :: N) a = Now { getNow :: C.Now a }
  deriving (Functor,Applicative,Monad, MonadFix, MonadIO)

-- | Sample the present value of a behavior
sampleNow :: Behavior n a -> Now n a
sampleNow = Now . C.sampleNow . getBehavior

-- | Create an event that occurs when the callback is called.
callback :: Now n (Event m a, a -> IO ())
callback = Now $ first Event <$> C.callback

-- | Synchronously execte an IO action.
sync :: IO a -> Now n a
sync = Now . C.sync

-- | Asynchronously execte an IO action, and obtain the event that it is done.
async :: IO a -> Now n (Event (S n) a)
async = Now . fmap Event . C.async

-- | Like 'async', but uses an OS thread instead of a regular lightweight thread.
asyncOS :: IO a -> Now n (Event (S n) a)
asyncOS = Now . fmap Event . C.asyncOS

-- | Plan to execute a 'Now' computation.
-- When given a event carrying a now computation, execute that now computation as soon as the event occurs.
-- If the event has already occured when 'planNow' is called, then the 'Now' computation will be executed immediatly.
planNow :: Event n (Now m a) -> Now m (Event n a)
planNow = Now . fmap Event . C.planNow . fmap getNow . getEvent

-- | General interface to interact with the FRP system.
initNow ::
      (IO (Maybe a) -> IO ()) -- ^ An IO action that schedules some FRP actions to be run. The callee should ensure that all actions that are scheduled are ran on the same thread. If a scheduled action returns @Just x@, then the ending event has occured with value @x@ and now more FRP actions are scheduled.
  ->  Now n (Event m a) -- ^ The @Now@ computation to execute, resulting in the ending event, i.e. the event that stops the FRP system.
  -> IO ()
initNow action = C.initNow action . fmap getEvent . getNow

-- | Run the FRP system in master mode.
runNowMaster :: Now n (Event m a) -> IO a
runNowMaster = C.runNowMaster . fmap getEvent . getNow

rewindTime :: (forall n. Behavior (S n) (Event (S (S n)) a))
           -> (forall m. Behavior    m  (Event    (S m)  a))
rewindTime b = Behavior $ fmap (Event . getEvent) $ getBehavior b
