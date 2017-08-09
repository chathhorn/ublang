{-# LANGUAGE MultiParamTypeClasses
      , FlexibleInstances
      , UndecidableInstances
      #-}
module UndefResumption where

import Control.Monad.Trans
import Control.Monad.State
import Control.Applicative

import StrongMonad
import qualified Resumption as R

data R m a = Undef | Computed a | Resume (m (R m a))
instance Monad m => Applicative (R m) where
      pure = return
      (<*>) = ap
instance Monad m => Functor (R m) where
      fmap f m = m >>= return . f
instance Monad m => Monad (R m) where
      return = Computed
      Undef      >>= _ = Undef
      Computed v >>= f = f v
      Resume m   >>= f = Resume (m >>= \r -> return (r >>= f))
instance MonadTrans R where
      lift m = Resume (fmap return m)
instance MonadState s m => MonadState s (R m) where
      state = stepR . state
instance (Monad m, Alternative m) => Alternative (R m) where
      empty = Undef
      Resume m1 <|> Resume m2 = Resume (m1        <|> m2)
      m1        <|> Resume m2 = Resume (return m1 <|> m2)
      Resume m1 <|> m2        = Resume (m1        <|> return m2)
      m1        <|> m2        = Resume (return m1 <|> return m2)
instance (Monad m, Alternative m) => StrongMonad (R m) where
      r1@(Resume m1) +:+ r2@(Resume m2) = Resume ((m1 >>= \r1' -> return (r1' +:+ r2)) <|> (m2 >>= \r2' -> return (r1 +:+ r2')))
      r1             +:+ r2             = (r1 >>= \v1 -> r2 >>= \v2 -> return (v1, v2)) <|> (r2 >>= \v2 -> r1 >>= \v1 -> return (v1, v2))

runUR :: Monad m => R m a -> R.R m (Maybe a)
runUR Undef        = return Nothing
runUR (Computed v) = return (Just v)
runUR (Resume m)   = R.Resume (fmap runUR m)

stepR :: Monad m => m a -> R m a
stepR m = Resume (fmap Computed m)
