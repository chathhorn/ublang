{-# LANGUAGE MultiParamTypeClasses
      , FlexibleInstances
      , FlexibleContexts
      , ScopedTypeVariables
      , UndecidableInstances
      , LambdaCase
      #-}

module SUBLang where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Function
import Data.Maybe
import qualified Data.Foldable as F
import Data.List.NonEmpty (NonEmpty(..), toList, groupAllWith1)
import qualified Data.List.NonEmpty as NEL
import Data.Semigroup
import Data.Functor.Classes

class Monad m => MultiMonad m where
      (+|+) :: m a -> m a -> m a

class Monad m => StrongMonad m where
      (+:+) :: m a -> m b -> m (a, b)

update i n s x = if i == x then n else s x

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

instance MultiMonad NonEmpty where
      (+|+) = (<>)

instance MultiMonad m => MultiMonad (StateT s m) where
      StateT r1 +|+ StateT r2 = StateT (\s -> r1 s +|+ r2 s)

instance MultiMonad m => MultiMonad (MaybeT m) where
      MaybeT r1 +|+ MaybeT r2 = MaybeT (r1 +|+ r2)

instance MultiMonad m => MultiMonad (R m) where
      (Computed v1) +|+ (Computed v2) = Resume (return (return v1) +|+ return (return v2))
      (Resume m1)   +|+ (Computed v2) = Resume (m1 +|+ return (return v2))
      (Computed v1) +|+ (Resume m2)   = Resume (return (return v1) +|+ m2)
      (Resume m1)   +|+ (Resume m2)   = Resume (m1 +|+ m2)

instance Alternative NonEmpty where
      empty = undefined
      (<|>) = (<>)
instance MonadPlus NonEmpty where
      mzero = undefined
      mplus = (<>)

runR :: Monad m => R m a -> m a
runR (Computed v) = return v
runR (Resume m) = m >>= runR

stepR :: Monad m => m a -> R m a
stepR m = Resume (fmap Computed m)

instance MonadState s m => MonadState s (R m) where
     state = stepR . state

type Abs = MaybeT RS
type RS = R (StateT S NonEmpty)

instance (MultiMonad m, MonadPlus m) => Alternative (R m) where
      empty = mzero
      (<|>) = mplus
instance (MultiMonad m, MonadPlus m) => MonadPlus (R m) where
      mzero = lift mzero
      mplus = (+|+)

instance MultiMonad m => StrongMonad (R m) where
      Computed v1 +:+ r2                = r2 >>= \v2 -> return (v1, v2)
      r1 +:+ Computed v2                = r1 >>= \v1 -> return (v1, v2)
      r1@(Resume m1) +:+ r2@(Resume m2) = Resume ((m1 >>= \r1' -> return (r1' +:+ r2)) +|+ (m2 >>= \r2' -> return (r1 +:+ r2')))


instance (MultiMonad m, StrongMonad m) => StrongMonad (MaybeT m) where
      ma +:+ mb = (do
                  a <- ma
                  b <- mb
                  return (a, b))
            +|+ (do
                  b <- mb
                  a <- ma
                  return (a, b))
            +|+ MaybeT ((runMaybeT ma) +:+ (runMaybeT mb) >>= \ case
                  (Just a, Just b) -> return (Just (a, b))
                  _ -> return Nothing
                  )

type N = Int

type Ide = String

commit :: S -> S
commit (S (sn, sx, o)) = S
      ( \ i -> fromMaybe (sn i) (sx i)
      , const Nothing
      , o
      )

seqpt :: MonadState S m => m ()
seqpt = do
      modify commit
      return ()

data Tree s a = Branch !s !(NonEmpty (Tree s a)) | Leaf !a
      deriving (Eq, Ord)

instance (Show s, Show a) => Show (Tree s a) where
      show = \ case
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
runRS s = \ case
      Resume p    -> Branch s (run' p)
      Computed a  -> Branch s (Leaf a :| [])
      where run' = fmap (uncurry (flip runRS)) . flip runStateT s

proj :: Ord a => RS a -> Tree S a
proj = merge . shrink . runRS initial

shrink :: (Eq s, Ord a) => Tree s a -> Tree s a
shrink (Leaf n)      = Leaf n
shrink (Branch s bs) = Branch s (shrink' s (Branch s bs))
      where shrink' :: (Eq s, Ord a) => s -> Tree s a -> NonEmpty (Tree s a)
            shrink' s = \ case
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
prune_minus = \ case
      Branch s bs | F.any isUB bs    -> Branch s (return (Leaf Nothing))
      Branch s bs                    -> Branch s (fmap prune_minus bs)
      leaf                           -> leaf
      where isUB = \ case
                  Leaf Nothing       -> True
                  Branch _ (b :| []) -> isUB b
                  t                  -> False

prune_bot :: Tree s (Maybe a) -> Tree s (Maybe a)
prune_bot t = if hasUB t then Leaf Nothing else t
      where hasUB = \ case
                  Leaf Nothing      -> True
                  Branch _ bs       -> any hasUB bs
                  _                 -> False

-- a <: b is True iff a is a refinement (or subtree) of b (rooted at the root
-- of b), with UB treated as a hole.
(<:) :: (Eq s, Eq a) => Tree s a -> Tree s (Maybe a) -> Bool
_             <: (Leaf Nothing)   = True
(Leaf n)      <: (Leaf (Just n')) = n == n'
(Branch s bs) <: (Branch s' bs')  = s == s' && all (or . flip fmap bs' . (<:)) bs
_             <: _                = False

