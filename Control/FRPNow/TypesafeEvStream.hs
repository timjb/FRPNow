{-# LANGUAGE ScopedTypeVariables,TypeOperators,MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  Control.FRPNow.EvStream
-- Copyright   :  (c) Atze van der Ploeg 2015
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Event streams for FRPNow

module Control.FRPNow.TypesafeEvStream() where

{-
module Control.FRPNow.TypesafeEvStream(
  EvStream,
  -- * Observe
  next, nextAll,
   -- * Construction
   emptyEs,
   merge,
   --collapseSimul,
   --dropEv,
   toChanges,
   edges,
   --joinEs,
   -- * Folds and scans
  scanlEv,
  --foldrEv,
  --foldriEv,
  fromChanges,
  --foldrSwitch,
  --foldEs,
  --foldBs,
  -- * Filter and scan
  catMaybesEs,filterEs,filterMapEs,filterMapEsB, filterB, during, beforeEs,
  -- * Combine behavior and eventstream
  (<@@>) , snapshots, --delay,
  -- * IO interface
  --callbackStream,callStream, callIOStream,
  -- * Debug
  --traceEs
  ) where

import Data.Maybe
import Control.Monad hiding (when)
import Control.Applicative hiding (empty)
import Data.IORef
import qualified Data.Sequence as Seq
import Prelude hiding (until,length)
import qualified Prelude as P
import Debug.Trace
import Data.Monoid

import Control.FRPNow.TypesafeCore hiding (S)
import qualified Control.FRPNow.TypesafeCore as C
import Control.FRPNow.TypesafeLib
import Debug.Trace

-- | The (abstract) type of event streams.
--
-- Denotationally, one can think of an eventstream a value
-- of type
--
-- > [(Time,a)]
--
-- Where the points in time are non-strictly increasing.
-- There can be multiple simulatinous events in an event stream.

newtype EvStream a = S { getEs :: forall n. Behavior n (Event (C.S n) [a]) }

instance Functor EvStream where
  fmap f (S b) = S $ (fmap f <$>) <$> b

instance Monoid (EvStream a) where
  mempty = emptyEs
  mappend = merge

-- | The empty event stream
emptyEs :: EvStream a
emptyEs = S $ pure never

-- | Merge two event stream.
--
-- In case of simultaneity, the left elements come first
merge :: EvStream a -> EvStream a -> EvStream a
merge l r = loop where
  loop = S $
   do l' <- getEs l
      r' <- getEs r
      e <- fmap nxt <$> cmpTime l' r'
      m <- lookEvent e
      case m of
        Left  _ -> getEs loop
        Right _ -> pure e
  nxt (Simul       l r) = l ++ r
  nxt (LeftEarlier   l) = l
  nxt (RightEarlier  r) = r

-- | Collapses each set of simultaneous events into a single event carrying the list of occurrences.
collapseSimul :: EvStream a -> EvStream [a]
collapseSimul (S s) = S $ ((\x -> [x]) <$>) <$> s

-- | Obtain the next element of the event stream. The obtained event is guaranteed to lie in the future.
next :: EvStream a -> Behavior n (Event (C.S n) a)
next s = (head <$>) <$> (nextAll s)

-- | Obtain all simultaneous next elements of the event stream. The obtained event is guaranteed to lie in the future.
nextAll :: EvStream a -> Behavior n (Event (C.S n) [a])
nextAll e = futuristic $  getEs e

-- | Sample the behavior each time an event in the stream
-- occurs, and combine the outcomes.
(<@@>) :: (forall n. Behavior n (a -> b)) -> EvStream a -> EvStream b
(<@@>) f es =
   S $ do e <- getEs es
          planB (flip fmap f . flip fmap <$> e)

-- | Sample the behavior each time an event in the stream
-- occurs.
snapshots :: Behavior n a -> EvStream () -> EvStream a
snapshots b s = S $
  do  e       <- getEs s
      ((\x -> [x]) <$>) <$> snapshot b (head <$> e)

-- | Get the event stream of changes to the input behavior.
toChanges :: Eq a => (forall n. Behavior n a) -> EvStream a
toChanges b = repeatEv (change b)

-- | Get the events that the behavior changes from @False@ to @True@
edges :: (forall n. Behavior n Bool) -> EvStream ()
edges b = repeatEv (edge b)



repeatEv :: (forall n. Behavior n (Event (C.S n) a)) -> EvStream a
repeatEv b =
   S $ do e <- b
          return $  (\x -> [x]) <$> e

-- | Create a behavior from an initial value and
-- a event stream of updates.
--
fromChanges :: a -> EvStream a -> Behavior n (Behavior (C.S n) a)
fromChanges i s = loop i where
  loop i = do e  <- nextAll s
              e' <- planB (loop . last <$> e)
              return (i `step` e')


dropEv :: Int -> EvStream a -> EvStream a
dropEv i (S s :: EvStream a) = S $ loop i where
  loop :: Int -> Behavior n (Event (C.S n) [a])
  loop 0 = s
  loop i = do e <- s
              join <$> planB (loop (i-1) <$ e)


-- | Filter the 'Just' values from an event stream.
--
catMaybesEs :: EvStream (Maybe a) -> EvStream a
catMaybesEs (s :: EvStream (Maybe a)) = S $ loop where
  loop :: Behavior n (Event (C.S n) [a])
  loop = do  e <- getEs s
             join <$> planB (nxt <$> e)
  nxt :: [Maybe a] -> Behavior n (Event (C.S n) [a])
  nxt l = case  catMaybes l of
             [] -> loop
             l  -> return (return l)

-- | Filter events from an event stream
--
filterEs :: (a -> Bool) -> EvStream a -> EvStream a
filterEs f s = catMaybesEs (toMaybef <$> s)
  where toMaybef x | f x = Just x
                   | otherwise = Nothing

-- | Shorthand for
--
-- > filterMapEs f e = catMaybesEs $ f <$> e
filterMapEs :: (a -> Maybe b) -> EvStream a -> EvStream b
filterMapEs f e = catMaybesEs $ f <$> e

-- | Shorthand for
--
-- > filterMapEs b e = catMaybesEs $ b <@@> e
--
filterMapEsB :: (forall n. Behavior n (a -> Maybe b)) -> EvStream a -> EvStream b
filterMapEsB f e = catMaybesEs $ f <@@> e

-- | Filter events from an eventstream based on a function that
-- changes over time
--
filterB :: (forall n. Behavior n (a -> Bool)) -> EvStream a -> EvStream a
filterB f = filterMapEsB (toMaybe <$> f)
  where toMaybe f = \a ->  if f a then Just a else Nothing

-- | Obtain only the events from input stream that occur while
-- the input behavior is 'True'
--
during :: EvStream a -> (forall n. Behavior n Bool) -> EvStream a
e `during` b = filterB (const <$> b) e

-- | A left scan over an event stream
--scanlEv :: (a -> b -> a) -> a -> EvStream b -> Behavior (EvStream a)
scanlEv f (i :: a) es = loop i where
 loop :: a -> Behavior m (EvStream a)
 loop i =
  do e  <- nextAll es
     let e' = (\(h : t) -> tail $ scanl f i (h : t)) <$> e
     ev <- planB (fmap getEs . loop . last <$> e')
     let x = pure e' `switch` ev
     return undefined
     --return $ S $ rewindTime $
-- nextAll :: EvStream a -> Behavior n (Event (C.S n) [a])
-- planB :: Event m (Behavior k a) -> Behavior n (Event (S n) a)
{-

-- | Turns an event of an event stream into an event stream.
joinEs :: Event (EvStream b) -> EvStream b
joinEs e = S $ before `switch` after where
  before = join <$> plan (getEs <$> e)
  after = getEs <$> e



-- | Left fold over an eventstream to create a behavior (behavior depends on when
-- the fold started).
foldEs :: (a -> b -> a) -> a -> EvStream b -> Behavior (Behavior a)
foldEs f i s = loop i where
  loop i = do e  <- nextAll s
              let e' = foldl f i <$> e
              ev <- plan (loop <$> e')
              return (i `step` ev)

-- | Right fold over an eventstream
--
-- The result of folding over the rest of the event stream is in an event,
-- since it can be only known in the future.
--
-- No initial value needs to be given, since the initial value is 'Control.FRPNow.Core.never'
foldrEv :: (a -> Event b -> b) -> EvStream a -> Behavior (Event b)
foldrEv f es = loop where
 loop =
  do e  <- nextAll es
     plan (nxt <$> e)
 nxt [h]     = f h          <$> loop
 nxt (h : t) = f h . return <$> nxt t


-- | Right fold over an eventstream with a left initial value
--
-- Defined as:
--
-- > foldriEv i f ev =  f i <$> foldrEv f es
foldriEv :: a -> (a -> Event b -> b) -> EvStream a -> Behavior b
foldriEv i f es = f i <$> foldrEv f es



-- | Start with the argument behavior, and switch to a new behavior each time
-- an event in the event stream occurs.
--
-- Defined as:
--
-- > foldrSwitch b = foldriEv b switch
--
foldrSwitch :: Behavior a -> EvStream (Behavior a) -> Behavior (Behavior a)
foldrSwitch b = foldriEv b switch

-- | Yet another type of fold.
--
-- Defined as:
--
-- > foldBs b f es = scanlEv f b es >>= foldrSwitch b
foldBs :: Behavior a -> (Behavior a -> b -> Behavior a) -> EvStream b -> Behavior (Behavior a)
foldBs b f es = scanlEv f b es >>= foldrSwitch b
-}
-- | An event stream with only elements that occur before the argument event.
beforeEs :: EvStream a -> (forall n. Event n ()) -> EvStream a
beforeEs (s :: EvStream a) e = S $ beforeEv `switch` (pure never <$ e)
  where beforeEv :: Behavior n (Event (C.S n) [a])
        beforeEv = do se <- getEs s
                      ev <- first (Left <$> e) (Right <$> se)
                      return (ev >>= choose)
        choose (Left _)  = never
        choose (Right x) = return x
{-

-- | Delay a behavior by one tick of the ``clock''.
--
-- The event stream functions as the ``clock'': the input behavior is sampled on each
-- event, and the current value of the output behavior is always the previously sample.
--
--  Occasionally useful to prevent immediate feedback loops.
delay ::  EvStream x -- ^ The event stream that functions as the ``clock''
          -> a -- ^ The inital value of the output behavior
          -> Behavior a  -- ^ The input behavior
          -> Behavior (Behavior a)
delay s i b = loop i where
  loop i =
           do e <- futuristic $
                        do cur <- b
                           e <- getEs s
                           return (cur <$ e)
              e' <- plan ( loop <$> e)
              return (i `step` e')

-- | Create an event stream that has an event each time the
-- returned function is called. The function can be called from any thread.
callbackStream :: forall a. Now (EvStream a, a -> IO ())
callbackStream = do mv <- sync $ newIORef ([], Nothing)
                    (_,s) <- loop mv
                    return (S s, func mv) where
  loop :: IORef ( [a], Maybe (() -> IO ()) ) -> Now ([a], Behavior (Event [a]))
  loop mv =
         do (l, Nothing) <- sync $ readIORef mv
            (e,cb) <- callback
            sync $ writeIORef mv ([], Just cb)
            es <- planNow $ loop mv <$ e
            let h = fst <$> es
            let t = snd <$> es
            return (reverse l, h `step` t)

  func mv x =
    do (l,mcb) <- readIORef mv
       writeIORef mv (x:l, Nothing)
       case mcb of
         Just x -> x ()
         Nothing -> return ()



-- | Call the given function each time an event occurs, and execute the resulting Now computation

callStream :: ([a] -> Now ()) -> EvStream a -> Now ()
callStream f evs = do e2 <- sample (nextAll evs)
                      planNow (again <$>  e2)
                      return () where
  again a = do f a
               e <- sample (nextAll evs)
               planNow (again <$> e)
               return ()


-- | Execute  the given IO action each time an event occurs. The IO action is executed on the main thread, so it should not take a long time.
callIOStream :: (a -> IO ()) -> EvStream a -> Now ()
callIOStream f = callStream (\x -> sync (mapM_ f x) >> return ())

-- | Debug function, print all values in the event stream to stderr, prepended with the given string.
traceEs :: (Show a, Eq a) => String -> EvStream a -> Now ()
traceEs s es = callIOStream (\x -> traceIO (s ++ show x)) es
-}
-}
