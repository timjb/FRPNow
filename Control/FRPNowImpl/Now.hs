{-# LANGUAGE ScopedTypeVariables,TypeSynonymInstances,Rank2Types,TupleSections,LambdaCase,ExistentialQuantification,GADTs,GeneralizedNewtypeDeriving #-}
module Control.FRPNowImpl.Now(Now, syncIO, asyncIO, evNow, firstObs, planIO, planIOWeak, planIOWeakKey, runFRPLocal, runFRP, Global) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Reader
import Debug.Trace
import Data.Maybe
import Control.ASync
import Control.FRPNowImpl.Event
import Data.IVar
import Control.Concurrent.ConcFlag
import System.IO.Unsafe
import Data.Ref
import System.Mem.Weak
import System.Mem



data Plan s = forall a. Plan (Event s (Now s a))  (IVar a)

newtype Now s a = Now { runNow' ::  ReaderT (MVar [Ref (Plan s)]) (ASync s) a } deriving (Functor, Applicative, Monad)

syncIO :: IO a -> Now s a
syncIO m = Now $ liftIO m

asyncIO :: IO a -> Now s (Event s a)
asyncIO m = Now $ makeEvent . observeAt <$> lift (async m)


evNow :: Event s a -> Now s (Maybe a)
evNow e = fmap snd . runEv e . Time <$> Now (lift $ prevRound)

firstObs :: Event s a -> Event s a -> Now s (Event s a)
firstObs l r = 
  do lv <- evNow l
     rv <- evNow r 
     return $ case (lv,rv) of
       (_, Just r) -> pure r
       (Just l, _) -> pure l
       (_, _     ) -> first l r

planIO :: Event s (Now s a) -> Now s (Event s a)
planIO  = planIO' (\_ y -> makeStrongRef y)

planIOWeak :: Event s (Now s a) -> Now s (Event s a)
planIOWeak  = planIO' makeWeakRefKey

planIOWeakKey :: k -> Event s (Now s a) -> Now s (Event s a)
planIOWeakKey k =  planIO' (\_ y -> makeWeakRefKey k y)

planIO' makeRef e = evNow e >>= \case
            Just n  -> pure <$> n
            Nothing -> do iv <- syncIO $ newIVar
                          let evRes = ivarVal iv <$ e
                          p <- syncIO $ makeRef iv (Plan e iv)
                          addPlan p
                          return evRes


addPlan :: Ref (Plan s) -> Now s ()
addPlan p = Now $
 do plmv <- ask
    pl <- liftIO $ takeMVar plmv
    liftIO $ putMVar plmv (p : pl)



tryPlan :: Ref (Plan s) -> Now s ()
tryPlan p = 
    syncIO (deRef p) >>= \case
       Just (Plan e iv) -> evNow e >>= \case
            Just n  -> n >>= syncIO . writeIVar iv
            Nothing -> addPlan p
       Nothing -> return ()



tryPlans :: Now s ()
tryPlans = Now $ 
  do plmv <- ask
     pl <- liftIO $ swapMVar plmv []
     -- let n = length $ filter isWeak pl
     -- liftIO $ putStrLn (show $ (n,length pl - n))
     mapM_ (parTryPlan plmv) pl
  where parTryPlan plmv p = 
         lift $ forkASync $ runReaderT (runNow' (tryPlan p)) plmv 

runFRPLocal :: (forall s. Now s (Event s a)) -> IO a
runFRPLocal m = runRoundM $ 
     do mv <- runASync $ liftIO $ newMVar []
        v <- runNow mv m
        loop mv v where
   loop mv v = 
    runNow mv (evNow v) >>= \case
         Just a -> return a
         Nothing -> 
           do waitEndRound 
              runNow mv tryPlans
              loop mv v

runNow mv n = runASync (runReaderT (runNow' n) mv)  

-- Global stuff

data Global 

data FRPInit = forall a. FRPInit (Now Global (Event Global a)) (MVar a)

runInits :: MVar [FRPInit] -> Now Global ()
runInits inits = 
  do is <- syncIO $ swapMVar inits []
     mapM_ runInit is
  where runInit (FRPInit n m) = 
          do e <- n
             let setEm a = syncIO (putMVar m a)
             planIO (setEm <$> e)

globalLoop :: MVar [Ref (Plan Global)] -> MVar [FRPInit] -> RoundM Global ()
globalLoop mv init = forever $ 
   do waitEndRound 
      runNow mv tryPlans
      runNow mv (runInits init)


global :: (Flag, MVar [FRPInit])
global = unsafePerformIO $
    do flag <- newFlag
       init <- newMVar []
       plans <- newMVar []
       forkIO $ unsafeRunRoundM flag (globalLoop plans init)
       return (flag, init)
{-# NOINLINE global #-}  

runFRP :: Now Global (Event Global a) -> IO a
runFRP n = do m <- newEmptyMVar 
              let (flag,inits) = global
              is <- takeMVar inits
              putMVar inits (FRPInit n m : is)
              signal flag
              takeMVar m
              

