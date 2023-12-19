{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad
import Control.Monad.Fail
import Control.Monad.Identity
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Reader

data Logged a = Logged String a deriving (Eq, Show)

newtype LoggT m a = LoggT {runLoggT :: m (Logged a)}

instance Functor Logged where
  fmap f (Logged s a) = Logged s (f a)

instance Applicative Logged where
  pure = Logged ""
  (<*>) = ap

instance Monad Logged where
  return = pure
  (>>=) (Logged s a) f = Logged (s' ++ s) y
                        where
                            Logged s' y = f a

instance Monad m => Functor (LoggT m) where
  fmap = liftM

instance Monad m => Applicative (LoggT m) where
  pure x = LoggT $ pure (pure x)
  (<*>) = ap

instance Monad m => Monad (LoggT m) where
  return = pure
  m >>= f = LoggT $ do
    Logged str x <- runLoggT m
    fmap (Logged str id <*>) $ runLoggT $ f x

instance MonadFail m => MonadFail (LoggT m) where
  fail msg = LoggT $ fail msg

--  эквивалент tell
write2log' s = Logged s () 

write2log s = LoggT $ return $ Logged s ()  

type Logg = LoggT Identity

runLogg s = runIdentity $ runLoggT s

instance MonadTrans LoggT where
  lift m = LoggT $ do
    Logged mempty <$> m

instance MonadState s m => MonadState s (LoggT m) where
  get   = lift get
  put   = lift . put
  state = lift . state

instance MonadReader r m => MonadReader r (LoggT m) where
  -- ask :: m r
  ask = lift ask

  --local :: (r -> r) -> m a -> m a
  local = mapLoggT . local

  --reader :: (r -> a) -> m a
  reader = lift . reader

mapLoggT :: (m (Logged a) -> n (Logged b)) -> LoggT m a -> LoggT n b
mapLoggT f = LoggT . f . runLoggT

class Monad m => MonadLogg m where
    w2log :: String -> m ()
    logg :: Logged a -> m a

instance Monad m => MonadLogg (LoggT m) where
    w2log = write2log
    logg  = LoggT . return

instance MonadLogg m => MonadLogg (StateT s m) where
    w2log = lift . w2log
    logg  = lift . logg

instance MonadLogg m => MonadLogg (ReaderT r m) where
    w2log = lift . w2log
    logg  = lift . logg