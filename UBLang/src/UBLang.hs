{-# LANGUAGE MultiParamTypeClasses
      , FlexibleInstances
      , ScopedTypeVariables
      , UndecidableInstances
      , LambdaCase
      #-}

module UBLang where

import Control.Monad.Trans.Either
import Control.Monad.State
import Data.Maybe
import Data.Function
import qualified Data.Foldable as F
import Data.List.NonEmpty (NonEmpty(..), toList)
import qualified Data.List.NonEmpty as NEL

class Monad m => MultiMonad m where
      (+|+) :: m a -> m a -> m a
class Monad m => StrongMonad m where
      (+:+) :: m a -> m b -> m (a, b)

update i n s x = if i == x then n else s x

class Sto s where
      initial :: s
      load :: Ide -> s -> Either String N
      store :: Ide -> N -> s -> Either String s
      load' :: Ide -> s -> N
      store' :: Ide -> N -> s -> s
      obs :: String -> s -> s

-- The last arg represents observable behavior.
newtype S = S (Ide -> N, Ide -> Maybe N, String)

instance Sto S where
      initial = S (const 0, const Nothing, "")
      load i (S (sn, sx, _)) = case sx i of
            Nothing -> Right $ sn i
            _ -> Left ("Store-load conflict for variable " ++ i)
      store i n (S (sn, sx, o)) = case sx i of
            Nothing -> Right $ S (sn, update i (Just n) sx, o)
            _ -> Left ("Store-store conflict for variable " ++ i)
      load' i (S (sn, _, _)) = sn i
      store' i n (S (sn, sx, o)) = commit $ S (sn, update i (Just n) sx, o)
      obs o' (S (sn, sx, o)) = S (sn, sx, o ++ o')
instance Show S where
      show (S (_, _, o)) = o
instance Eq S where
      (S (_, _, o)) == (S (_, _, o')) = o == o'
instance Ord S where
      (S (_, _, o)) <= (S (_, _, o')) = o <= o'

instance MultiMonad [] where
      (+|+) = (++)

instance MultiMonad NonEmpty where
      (a :| as) +|+ (b :| bs) = a :| (as ++ (b:bs))

instance MultiMonad m => MultiMonad (EitherT String m) where
      (EitherT a) +|+ (EitherT b) = EitherT (a +|+ b)

instance MultiMonad m => MultiMonad (StateT s m) where
      StateT r1 +|+ StateT r2 = StateT (\s -> r1 s +|+ r2 s)

instance MultiMonad m => StrongMonad (StateT s m) where
      d1 +:+ d2 =
            (d1 >>= \v1 -> d2 >>= \v2 -> return (v1, v2)) +|+
            (d2 >>= \v2 -> d1 >>= \v1 -> return (v1, v2))

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

runR :: Monad m => R m a -> m a
runR (Computed v) = return v
runR (Resume m) = m >>= runR

stepR :: Monad m => m a -> R m a
stepR m = Resume (fmap Computed m)

instance MonadState s m => MonadState s (R m) where
     state = stepR . state

type Abs = StateT S (EitherT String NonEmpty)
type Conc = StateT S NonEmpty

instance MultiMonad m => MultiMonad (R m) where
      (Computed v1) +|+ (Computed v2)     = Resume (return (return v1) +|+ return (return v2))
      (Resume m1)   +|+ (Computed v2)     = Resume (m1 +|+ return (return v2))
      (Computed v1) +|+ (Resume m2)       = Resume (return (return v1) +|+ m2)
      (Resume m1)   +|+ (Resume m2)       = Resume (m1 +|+ m2)

instance MultiMonad m => StrongMonad (R m) where
      Computed v1 +:+ r2 = r2 >>= \v2 -> return (v1, v2)
      r1 +:+ Computed v2 = r1 >>= \v1 -> return (v1, v2)
      r1@(Resume m1) +:+ r2@(Resume m2) = Resume ((m1 >>= \r1' -> return (r1' +:+ r2)) +|+ (m2 >>= \r2' -> return (r1 +:+ r2')))

type N = Int

type Ide = String

commit :: S -> S
commit (S (sn, sx, o)) = S
      ( \ i -> fromMaybe (sn i) (sx i)
      , const Nothing
      , o
      )

seqpt :: R Abs ()
seqpt = do
      modify commit
      return ()

seqpt' :: R Conc ()
seqpt' = do
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

runAbs :: S -> R Abs a -> Tree S (Maybe a)
runAbs s = \ case
      Resume p    -> Branch s (run' p)
      Computed a  -> Branch s (Leaf (Just a) :| [])
      where run' :: Abs (R Abs a) -> NonEmpty (Tree S (Maybe a))
            run' = eitherT (const (return (Leaf Nothing))) return . fmap (uncurry (flip runAbs)) . flip runStateT s

runConc :: S -> R Conc a -> Tree S a
runConc s = \ case
      Resume p    -> Branch s (run' p)
      Computed a  -> Branch s (Leaf a :| [])
      where run' :: Conc (R Conc a) -> NonEmpty (Tree S a)
            run' = fmap (uncurry (flip runConc)) . flip runStateT s

aproj :: (Ord a) => R Abs a -> Tree S (Maybe a)
aproj = norm . runAbs initial

cproj :: (Ord a) => R Conc a -> Tree S a
cproj = norm . runConc initial

norm :: (Eq s, Ord a) => Tree s a -> Tree s a
norm (Leaf n)      = Leaf n
norm (Branch s bs) = Branch s (norm' s (Branch s bs))
      where norm' :: (Eq s, Ord a) => s -> Tree s a -> NonEmpty (Tree s a)
            norm' s = \ case
                  Branch s' bs | s == s'    -> merge $ join $ fmap (norm' s) bs
                  Branch s' bs                   -> return (Branch s' (merge $ join $ fmap (norm' s') bs))
                  Leaf n                         -> return (Leaf n)

-- Merges subtrees with common prefix.
merge :: (Eq s, Eq a) => NonEmpty (Tree s a) -> NonEmpty (Tree s a)
merge = fmap merge' . NEL.groupBy1 ((==) `on` label)
      where merge' :: NonEmpty (Tree s a) -> Tree s a
            merge' (Branch s (b :| bs) :| bs') = Branch s (b :| bs ++ join (fmap children bs'))
            merge' (b :| _)                    = b

prune_top :: Tree s (Maybe a) -> Tree s (Maybe a)
prune_top = id

prune_minus :: Tree s (Maybe a) -> Tree s (Maybe a)
prune_minus = \ case
      Branch s bs | F.any isUB bs         -> Branch s (return (Leaf Nothing))
      Branch s bs                         -> Branch s (fmap prune_minus bs)
      leaf                                -> leaf
      where isUB = \ case
                  Leaf Nothing            -> True
                  Branch _ (b :| [])      -> isUB b
                  t                       -> False

prune_bot :: Tree s (Maybe a) -> Tree s (Maybe a)
prune_bot t = if hasUB t then Leaf Nothing else t
      where hasUB = \ case
                  Leaf Nothing      -> True
                  Branch _ bs       -> any hasUB bs
                  _                 -> False

-- a <: b is True iff a is a refinement (or subtree) of b (rooted at the root
-- of b), with UB treated as a hole.
(<:) :: (Eq s, Eq a) => Tree s a -> Tree s (Maybe a) -> Bool
_             <: (Leaf Nothing)           = True
(Leaf n)      <: (Leaf (Just n'))         = n == n'
(Branch s bs) <: (Branch s' bs')          = s == s' && all (or . flip fmap bs' . (<:)) bs
_             <: _                        = False

