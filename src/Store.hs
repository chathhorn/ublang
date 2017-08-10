module Store where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import Data.List.NonEmpty (NonEmpty(..), toList, groupAllWith1)
import Data.Maybe
import Data.Semigroup

type StL = StateT S NonEmpty

instance Alternative NonEmpty where
      empty = undefined
      (<|>) = (<>)
instance MonadPlus NonEmpty where
      mzero = undefined
      mplus = (<>)

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

