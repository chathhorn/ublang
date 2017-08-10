{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module UBLang where

import qualified UndefResumption as U
import Resumption
import Store

import Control.Applicative
import Control.Monad.State
import Data.List.NonEmpty (NonEmpty(..))

undef :: Alternative m => m a
undef = empty

seqpt :: MonadState S m => m ()
seqpt = do
      modify commit
      return ()

prune_top :: Tree s (Maybe a) -> Tree s (Maybe a)
prune_top = id

prune_minus :: Tree s (Maybe a) -> Tree s (Maybe a)
prune_minus = \case
      Branch s bs | any isUB bs -> Branch s (return (Leaf Nothing))
      Branch s bs               -> Branch s (fmap prune_minus bs)
      leaf                      -> leaf
      where isUB = \case
                  Leaf Nothing       -> True
                  Branch _ (b :| []) -> isUB b
                  t                  -> False

prune_bot :: Tree s (Maybe a) -> Tree s (Maybe a)
prune_bot t = if hasUB t then Leaf Nothing else t
      where hasUB = \case
                  Leaf Nothing      -> True
                  Branch _ bs       -> any hasUB bs
                  _                 -> False

-- a <: b is True iff a is a refinement (or subtree) of b (rooted at the root
-- of b), with UB treated as a hole.
(<:) :: (Eq s, Eq a) => Tree s a -> Tree s (Maybe a) -> Bool
_           <: Leaf Nothing   = True
Leaf n      <: Leaf (Just n') = n == n'
Branch s bs <: Branch s' bs'  = s == s' && all (flip any bs' . (<:)) bs
_           <: _              = False
