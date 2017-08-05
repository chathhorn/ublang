{-# LANGUAGE MultiParamTypeClasses
      , FlexibleInstances
      , ScopedTypeVariables
      , UndecidableInstances
      , DeriveGeneric
      #-}

module UBExp where

import SUBLang
import Test.QuickCheck
import Control.Monad.State
import Control.Monad.Trans.Maybe
import GHC.Generics hiding (S, R)

data Expr = E_int N | E_ide Ide | E_assign Ide Expr | E_plus Expr Expr
          | E_neg Expr | E_comma Expr Expr | E_unit Expr | E_call Expr
          | E_undef Ide | E_obs String
          deriving (Show, Generic)

instance Arbitrary Expr where
      shrink = genericShrink
      arbitrary = sized arb
            where arb n | n <= 1 = oneof
                        [ return $ E_int 0
                        , return $ E_int 1
                        , return $ E_ide "x"
                        , return $ E_ide "y"
                        , return $ E_undef "x"
                        , return $ E_undef "y"
                        , return $ E_obs "A"
                        , return $ E_obs "B"
                        ]
                  arb n = oneof
                        [ E_assign "x" <$> arb (n - 1)
                        , E_assign "y" <$> arb (n - 1)
                        , E_plus <$> arb (n `div` 2) <*> arb (n `div` 2)
                        , E_neg <$> arb (n - 1)
                        , E_comma <$> arb (n `div` 2) <*> arb (n `div` 2)
                        , E_unit <$> arb (n - 1)
                        , E_call <$> arb (n - 1)
                        ]


pp :: Expr -> String
pp e_ = case e_ of
      E_int n       -> show n
      E_ide x       -> x
      E_assign x e  -> x ++ " = " ++ pp e
      E_plus x y    -> pp x ++ " + " ++ pp y
      E_neg e       -> "-" ++ pp e
      E_comma x y   -> "(" ++ pp x ++ ", " ++ pp y ++ ")"
      E_unit e      -> "<" ++ pp e ++ ">"
      E_call e      -> "#" ++ pp e
      E_undef x     -> x ++ "?"
      E_obs s       -> "(obs: " ++ s ++ ")"

-- undef :: Abs a
undef = mzero

evalAbs :: Expr -> Abs N
evalAbs (E_int n) = return n
evalAbs (E_ide x) = do
      s <- get
      case load x s of
            Nothing -> undef
            Just n -> return n
evalAbs (E_assign x e) = do
      n <- evalAbs e
      s <- get
      case store x n s of
            Nothing -> undef
            Just s' -> put s' >> return n
evalAbs (E_plus e1 e2) = do
      (n1, n2) <- evalAbs e1 +:+ evalAbs e2
      return (n1 + n2)
evalAbs (E_neg e) = do
      n <- evalAbs e
      return (-n)
evalAbs (E_comma e1 e2) = do
      evalAbs e1
      seqpt
      evalAbs e2
evalAbs (E_unit e) = (MaybeT . stepR . runR . runMaybeT . evalAbs) e
evalAbs (E_call e) = do
      seqpt
      n <- evalAbs e
      seqpt
      return n
evalAbs (E_undef x) = do
      n <- evalAbs (E_ide x)
      if n == 0 then undef else return n
evalAbs (E_obs o) = do
      modify (obs o :: S -> S)
      return 1

evalConc :: Expr -> RS N
evalConc (E_int n) = return n
evalConc (E_ide x) = do
      s <- get
      case load x s of
            Nothing -> return 0
            Just n -> return n
evalConc (E_assign x e) = do
      n <- evalConc e
      s <- get
      case store x n s of
            Nothing -> return n
            Just s' -> put s' >> return n
evalConc (E_plus e1 e2) = do
      (n1, n2) <- evalConc e1 +:+ evalConc e2
      return (n1 + n2)
evalConc (E_neg e) = do
      n <- evalConc e
      return (-n)
evalConc (E_comma e1 e2) = do
      evalConc e1
      seqpt
      evalConc e2
evalConc (E_unit e) = stepR (runR (evalConc e))
evalConc (E_call e) = do
      seqpt
      n <- evalConc e
      seqpt
      return n
evalConc (E_undef x) = evalConc (E_ide x)
evalConc (E_obs o) = do
      modify (obs o)
      return 1

abs = proj . runMaybeT . evalAbs
conc = proj . evalConc

ex3 = E_comma
      (E_assign "x" (E_int 0))
      (E_plus
            (E_ide "x")
            (E_unit (E_comma
                (E_assign "x" (E_int 1))
                (E_comma (E_assign "x" (E_int 2)) (E_int 0)))))

ex4 = E_comma
      (E_assign "x" (E_int 0))
      (E_plus
          (E_unit (E_call (E_ide "x")))
          (E_assign "x" (E_int 1)))

ex5 = E_comma
      (E_assign "x" (E_int 0))
      (E_plus
            (E_unit (E_call (E_ide "x")))
            (E_comma
                  (E_assign "x" (E_int 1))
                  (E_comma
                        (E_assign "x" (E_int 2))
                        (E_int 0)
                  )
            )
      )

ub1 = E_comma
      (E_assign "x" (E_int 1))
      (E_plus
            (E_comma (E_obs "a") (E_assign "x" (E_int 0)))
            (E_comma (E_obs "b") (E_unit $ E_call $ E_undef "x"))
      )


ub2 = E_unit ( E_comma
            ( E_plus (E_obs "a") (E_int 0) )
            ( E_comma (E_undef "") (E_obs "a") )
      )
