{-# LANGUAGE MultiParamTypeClasses
      , FlexibleInstances
      , FlexibleContexts
      , UndecidableInstances
      , FunctionalDependencies
      , LambdaCase
      #-}

module Resumption where

import Store
import StrongMonad

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.State
import Data.List.NonEmpty (NonEmpty(..), toList, groupAllWith1)
import Data.Types.Injective

data R m a = Computed a | Resume (m (R m a))
instance Monad m => Applicative (R m) where
      pure = return
      (<*>) = ap
instance Monad m => Functor (R m) where
      fmap f m = m >>= return . f
instance Monad m => Monad (R m) where
      return = Computed
      Computed v >>= f = f v
      Resume m >>= f = Resume (m >>= \r -> return (r >>= f))
instance MonadTrans R where
      lift m = Resume (fmap return m)
instance MonadState s m => MonadState s (R m) where
      state = stepR . state
instance (Monad m, Alternative m) => Alternative (R m) where
      empty = undefined
      Resume m1 <|> Resume m2 = Resume (m1        <|> m2)
      m1        <|> Resume m2 = Resume (return m1 <|> m2)
      Resume m1 <|> m2        = Resume (m1        <|> return m2)
      m1        <|> m2        = Resume (return m1 <|> return m2)
instance (Monad m, Alternative m) => StrongMonad (R m) where
      r1@(Resume m1) +:+ r2@(Resume m2) = Resume ((m1 >>= \r1' -> return (r1' +:+ r2)) <|> (m2 >>= \r2' -> return (r1 +:+ r2')))
      r1             +:+ r2             = (r1 >>= \v1 -> r2 >>= \v2 -> return (v1, v2)) <|> (r2 >>= \v2 -> r1 >>= \v1 -> return (v1, v2))

data Tree s a = Leaf !a | Branch !s !(NonEmpty (Tree s a))
      deriving (Eq, Ord)

instance (Show s, Show a) => Show (Tree s a) where
      show = \case
            Branch s bs | show s == "" -> "Br " ++ show (toList bs)
            Branch s bs                -> "Br " ++ show s ++ " " ++ show (toList bs)
            Leaf n                     -> show n

children :: Tree s a -> [Tree s a]
children (Branch _ (b :| bs)) = b : bs
children _ = []

label :: Tree s a -> Either s a
label (Branch s _) = Left s
label (Leaf a) = Right a

class Resumption r a b | r b -> a where
      runR :: Monad m => r m a -> m b
      stepR :: Monad m => m b -> r m a
      proj :: Ord a => r StL a -> Tree S b

instance Resumption R a a where
      runR (Computed v) = return v
      runR (Resume m)   = m >>= runR
      stepR = Resume . fmap Computed
      proj = merge . shrink . unfold initial . to

unfold :: S -> R StL a -> Tree S a
unfold s = \case
      Resume p    -> Branch s (unfold' p)
      Computed a  -> Branch s (Leaf a :| [])
      where unfold' = fmap (uncurry $ flip unfold) . flip runStateT s

shrink :: (Eq s, Ord a) => Tree s a -> Tree s a
shrink (Leaf n)      = Leaf n
shrink (Branch s bs) = Branch s (shrink' s (Branch s bs))
      where shrink' :: (Eq s, Ord a) => s -> Tree s a -> NonEmpty (Tree s a)
            shrink' s = \case
                  Branch s' bs | s == s' -> join $ fmap (shrink' s) bs
                  Branch s' bs           -> return (Branch s' (join $ fmap (shrink' s') bs))
                  Leaf n                 -> return (Leaf n)

-- Merges subtrees with common prefix.
merge :: (Ord s, Ord a) => Tree s a -> Tree s a
merge (Leaf n)      = Leaf n
merge (Branch s bs) = Branch s (fmap merge' (groupAllWith1 label bs))
      where merge' :: (Ord s, Ord a) => NonEmpty (Tree s a) -> Tree s a
            merge' (Branch s (b :| bs) :| bs') = merge $ Branch s (b :| bs ++ join (fmap children bs'))
            merge' (b :| _)                    = b
