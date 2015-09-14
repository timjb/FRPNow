{-# LANGUAGE DoAndIfThenElse, FlexibleInstances , MultiParamTypeClasses,GADTs, TypeOperators, TupleSections, ScopedTypeVariables,ConstraintKinds,FlexibleContexts,UndecidableInstances,DataKinds #-}
--{-# LANGUAGE ScopedTypeVariables #-}

module Control.FRPNow.TypesafeLib (
   -- * Behavior construction
   step,
   cstep,
   -- * Getting events from behaviors
   when,
   change,
   edge,
   -- * Events and their ordering
   tryGetEv,
   hasOccured,
   first,
   cmpTime,
   EvOrd(..),
   -- * Fold and state
   prev,
   foldB,
   sampleUntil,
   -- * Sample behaviors on events
   planB,
   snapshot,
   (<@>),
   -- * Type classes for uniform interface
   --Plan(..),
   --Sample(..),
   -- * Debugging
   traceChanges
) where

import Control.FRPNow.TypesafeCore
import Control.Applicative
import Control.Monad hiding (when)
import Prelude hiding (until)
import Debug.Trace

-- | Start with a constant and then switch
--
-- Defined as:
--
-- > step a s = pure a `switch` s
step :: a -> Event n (Behavior k a) -> Behavior n a
step a s = pure a `switch` s


-- | Start with a constant, and switch to another constant when the event arrives.
--
-- Defined as:
--
-- >  cstep x e y = pure x `switch` (pure y <$ e)
cstep :: a -> Event n x -> a -> Behavior n a
cstep x e y = pure x `switch` (pure y <$ e)

-- | Like 'Control.FRPNow.whenJust' but on behaviors of type @Bool@ instead of @Maybe@.
--
--  Gives the event that the input behavior is @True@
when :: Behavior m Bool -> Behavior n (Event (S n) ())
when b = whenJust (boolToMaybe <$> b) where
  boolToMaybe True   = Just ()
  boolToMaybe False  = Nothing

-- | Gives the previous value of the behavior, starting with given value.
--
--  This /cannot/ be used to prevent immediate feedback loop! Use 'Control.FRPNow.EvStream.delay' instead!
prev :: Eq a => a -> Behavior n a -> Behavior n (Behavior (S n) a)
prev (i :: a) b = loop i where
  loop lastVal = do
    currVal <- b
    e <- whenJust (notSame currVal <$> b)
    (lastVal `step`) <$> snapshot (loop currVal) e
  notSame v v' | v /= v'   = Just v'
               | otherwise = Nothing
  {-
  loop :: a -> Behavior m (Behavior (S m) a)
  loop i = (i `step`) <$> nxtCur
  nxtCur :: Behavior m (Event (S m) (Behavior m a))
  nxtCur = futuristic $
             do (cur :: a) <- b
                (e :: Event (S m) a) <- change b
                --pure $ pure $ pure cur
                planB undefined
                --planB (loop cur <$ e)
  -}
-- planB :: Event n (Behavior k a) -> Behavior n (Event (S n) a)
-- step :: a -> Event n (Behavior k a) -> Behavior n a
-- futuristic :: Behavior n (Event (S n) a) -> Behavior n (Event (S n) a)
-- snapshot :: Behavior k a -> Event m b -> Behavior n (Event (S n) a)

-- | Gives at any point in time the event that the input behavior changes, and the new value of the input behavior.
change :: Eq a => Behavior n a -> Behavior n (Event (S n) a)
change b = futuristic $
           do v <- b ;
              whenJust (notSame v <$> b) where
    notSame v v' | v /= v'   = Just v'
                 | otherwise = Nothing

-- | The resulting behavior gives at any point in time, the event that the input
-- behavior next /becomes/ true. I.e. the next event that there is an edge from False to True. If the input behavior is True already, the event gives the
-- time that it is True again, after first being False for a period of time.
edge :: Behavior n Bool -> Behavior n (Event (S n) ())
edge b = futuristic $
             b >>= \v ->
              if v then (do e <- when (not <$> b)
                            join <$> planB (when b <$ e))
              else when b

-- | A (left) fold over a behavior.
--
-- The inital value of the resulting behavior is @f i x@ where @i@ the initial value given, and @x@ is the current value of the behavior.
--
foldB :: Eq a => (b -> a -> b) -> b -> Behavior n a -> Behavior n (Behavior (S n) b)
foldB f i b = loop i where
  loop i = do  c   <- b
               let i' = f i c
               e   <-  change b
               e'  <-  snapshot (loop i') (() <$ e)
               return (pure i' `switch` e')

-- | When sampled at a point in time t, the behavior gives an event with
-- the list of all values of the input behavior between time t and the
-- time that the argument event occurs (including the value when the event occurs).
sampleUntil :: Eq a => Behavior n a -> Event n () -> Behavior n (Event (S n) [a])
sampleUntil b end  = loop [] where
  loop ss = do s <- b
               let ss' = s : ss
               e <- hasOccured end
               if e then return (pure (reverse ss'))
               else do c <- change b
                       join <$> planB (loop ss' <$ c)

-- | Convert an event into a behavior that gives
-- @Nothing@ if the event has not occured yet, and @Just@ the value of the event if the event has already occured.
tryGetEv :: Event m a -> Behavior n (Maybe a)
tryGetEv = fmap f . lookEvent
  where f (Left v)  = Just v
        f (Right _) = Nothing

-- | The resulting behavior states wheter the input event has already occured.
hasOccured :: Event n x -> Behavior n Bool
hasOccured e = False `step` (pure True <$ e)

-- | Gives the first of two events.
--
-- If either of the events lies in the future, then the result will be the first of these events.
-- If both events have already occured, the left event is returned.
first :: Event m a -> Event k a -> Behavior n (Event (S n) a)
first l r = whenJust (tryGetEv r `switch` ((pure . Just) <$> l))

-- | Compare the time of two events.
--
-- The resulting behavior gives an event, occuring at the same time
-- as the earliest input event, of which the value indicates if the event where
-- simultanious, or if one was earlier.
--
-- If at the time of sampling both event lie in the past, then
-- the result is that they are simulatinous.
cmpTime :: Event m a -> Event k b -> Behavior n (Event (S n) (EvOrd a b))
cmpTime l r = whenJust (outcome <$> tryGetEv l <*> tryGetEv r) where
  outcome Nothing  Nothing  = Nothing
  outcome (Just x) Nothing  = Just (LeftEarlier x)
  outcome Nothing  (Just y) = Just (RightEarlier y)
  outcome (Just x) (Just y) = Just (Simul x y)

-- | The outcome of a 'cmpTime': the events occur simultanious, left is earlier or right is earlier.
data EvOrd l r = Simul l r
               | LeftEarlier l
               | RightEarlier r

-- | Plan to sample the behavior carried by the event as soon as possible.
--
-- If the resulting behavior is sampled after the event occurs,
-- then the behavior carried by the event will be sampled now.
planB :: Event m (Behavior k a) -> Behavior n (Event (S n) a)
planB e =
  whenJust (pure Nothing `switch` ((Just <$>) <$> e))

-- | Obtain the value of the behavior at the time the event occurs
--
-- If the event has already occured when sampling the resulting behavior,
-- we sample not the past, but the current value of the input behavior.
snapshot :: Behavior k a -> Event m b -> Behavior n (Event (S n) a)
snapshot b e =  let e' = (Just <$> b) <$ e
                in whenJust (pure Nothing `switch` e')

-- | Like 'snapshot', but feeds the result of the event to the
-- value of the given behavior at that time.
(<@>) :: Behavior k (a -> b) -> Event m a -> Behavior n (Event (S n) b)
b <@> e = planB $ fmap (\x -> b <*> pure x) e





{-
-- | A type class to unifying 'planNow' and 'planB'
class Monad b => Plan b where
  plan :: Event n (b a) -> b (Event n a)

instance Plan (Now m) where plan = planNow
instance Plan (Behavior m) where plan = planB

-- | A type class for behavior-like monads, such 'Now' and the monads from "Control.FRPNow.BehaviorEnd"
class Monad n => Sample n where
   sample :: Behavior a -> n a

instance Sample Behavior where sample = id
instance Sample Now where sample = sampleNow
-}




-- | A debug function, prints all values of the behavior to stderr, prepended with the given string.
traceChanges :: (Eq a, Show a) => String -> Behavior n a -> Now n ()
traceChanges s b = loop where
 loop = do v <- sampleNow b
           sync $ traceIO (s ++ show v)
           e <- sampleNow $ change b
           planNow (loop <$ e)
           return ()
