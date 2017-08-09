{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module UBLang where

import Control.Applicative
import Control.Monad.State
import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..), toList, groupAllWith1)
import Data.Semigroup

import Resumption
import qualified UndefResumption as U

class Sto s where
      initial :: s
      load :: Ide -> s -> Maybe N
      store :: Ide -> N -> s -> Maybe s
      load' :: Ide -> s -> N
      store' :: Ide -> N -> s -> s
      obs :: String -> s -> s

-- The last arg represents observable behavior.
newtype S = S (Ide -> N, Ide -> Maybe N, String)
instance Sto S where
      initial = S (const 0, const Nothing, "")
      load i (S (sn, sx, _)) = case sx i of
            Nothing -> Just $ sn i
            _ -> Nothing
      store i n (S (sn, sx, o)) = case sx i of
            Nothing -> Just $ S (sn, update i (Just n) sx, o)
            _ -> Nothing
      load' i (S (sn, _, _)) = sn i
      store' i n (S (sn, sx, o)) = commit $ S (sn, update i (Just n) sx, o)
      obs o' (S (sn, sx, o)) = S (sn, sx, o ++ o')
instance Show S where
      show (S (_, _, o)) = o
instance Eq S where
      (S (_, _, o)) == (S (_, _, o')) = o == o'
instance Ord S where
      (S (_, _, o)) <= (S (_, _, o')) = o <= o'

instance Alternative NonEmpty where
      empty = undefined
      (<|>) = (<>)
instance MonadPlus NonEmpty where
      mzero = undefined
      mplus = (<>)

type Abs = U.R (StateT S NonEmpty)
type RS = R (StateT S NonEmpty)
type N = Int
type Ide = String

update :: Ide -> a -> (Ide -> a) -> Ide -> a
update i n s x = if i == x then n else s x

commit :: S -> S
commit (S (sn, sx, o)) = S
      ( \i -> fromMaybe (sn i) (sx i)
      , const Nothing
      , o
      )

undef :: Alternative m => m a
undef = empty

seqpt :: MonadState S m => m ()
seqpt = do
      modify commit
      return ()

data Tree s a = Branch !s !(NonEmpty (Tree s a)) | Leaf !a
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

runRS :: S -> RS a -> Tree S a
runRS s = \case
      Resume p    -> Branch s (run' p)
      Computed a  -> Branch s (Leaf a :| [])
      where run' = fmap (uncurry (flip runRS)) . flip runStateT s

proj :: Ord a => RS a -> Tree S a
proj = merge . shrink . runRS initial

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
