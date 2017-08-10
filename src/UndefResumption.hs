{-# LANGUAGE MultiParamTypeClasses
      , FlexibleInstances
      , UndecidableInstances
      #-}

module UndefResumption where

import qualified Resumption as R
import Resumption (Resumption(..))
import StrongMonad

import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans
import Data.Types.Injective

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
      state = (Resume . fmap Computed) . state
instance (Monad m, Alternative m) => Alternative (R m) where
      empty = Undef
      Resume m1 <|> Resume m2 = Resume (m1        <|> m2)
      m1        <|> Resume m2 = Resume (return m1 <|> m2)
      Resume m1 <|> m2        = Resume (m1        <|> return m2)
      m1        <|> m2        = Resume (return m1 <|> return m2)
instance (Monad m, Alternative m) => StrongMonad (R m) where
      r1@(Resume m1) +:+ r2@(Resume m2) = Resume ((m1 >>= \r1' -> return (r1' +:+ r2)) <|> (m2 >>= \r2' -> return (r1 +:+ r2')))
      r1             +:+ r2             = (r1 >>= \v1 -> r2 >>= \v2 -> return (v1, v2)) <|> (r2 >>= \v2 -> r1 >>= \v1 -> return (v1, v2))

instance Functor m => Injective (R.R m (Maybe a)) (R m a) where
      to (R.Computed Nothing)  = Undef
      to (R.Computed (Just v)) = Computed v
      to (R.Resume m)          = Resume (fmap to m)

instance Functor m => Injective (R m a) (R.R m (Maybe a)) where
      to Undef        = R.Computed Nothing
      to (Computed v) = R.Computed (Just v)
      to (Resume m)   = R.Resume (fmap to m)

instance Resumption R a (Maybe a) where
      runR = runR . (to :: Functor m => R m a -> R.R m (Maybe a))
      stepR = (to :: Functor m => R.R m (Maybe a) -> R m a) . stepR
      proj = proj . (to :: Functor m => R m a -> R.R m (Maybe a))

