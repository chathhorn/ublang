module StrongMonad where

import Control.Applicative

class StrongMonad m where
      (+:+) :: m a -> m b -> m (a, b)

(+|+) :: Alternative m => m a -> m a -> m a
(+|+) = (<|>)

