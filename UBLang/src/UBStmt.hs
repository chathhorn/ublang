{-# LANGUAGE MultiParamTypeClasses
      , FlexibleInstances
      , ScopedTypeVariables
      , UndecidableInstances
      , LambdaCase
      , DeriveGeneric
      #-}

module UBStmt where

import UBLang
import Test.QuickCheck
import Control.Monad.State
import Control.Monad.Trans.Either
import GHC.Generics hiding (S, R)

data Expr = E_plus Expr Expr | E_minus Expr Expr | E_or Expr Expr | E_and Expr Expr
          | E_not Expr | E_int N | E_ide Ide | E_choose Expr Expr
          deriving (Show, Generic)

instance Arbitrary Expr where
      shrink = genericShrink
      arbitrary = sized arb
            where arb n | n <= 1 = oneof
                        [ return $ E_int 0
                        , return $ E_int 1
                        , return $ E_ide "x"
                        , return $ E_ide "y"
                        ]
                  arb n = oneof
                        [ E_plus <$> arb (n `div` 2) <*> arb (n `div` 2)
                        , E_minus <$> arb (n `div` 2) <*> arb (n `div` 2)
                        , E_or <$> arb (n `div` 2) <*> arb (n `div` 2)
                        , E_and <$> arb (n `div` 2) <*> arb (n `div` 2)
                        , E_not <$> arb (n - 1)
                        , E_choose <$> arb (n `div` 2) <*> arb (n `div` 2)
                        ]

ppExpr :: Expr -> String
ppExpr = \ case
      E_plus a b   -> "(" ++ ppExpr a ++ " + " ++ ppExpr b ++ ")"
      E_minus a b  -> "(" ++ ppExpr a ++ " - " ++ ppExpr b ++ ")"
      E_or a b     -> "(" ++ ppExpr a ++ " | " ++ ppExpr b ++ ")"
      E_and a b    -> "(" ++ ppExpr a ++ " & " ++ ppExpr b ++ ")"
      E_not e      -> "~" ++ ppExpr e
      E_int n      -> show n
      E_ide x      -> x
      E_choose a b -> "(" ++ ppExpr a ++ " [+] " ++ ppExpr b ++ ")"

data Stmt = S_assign Ide Expr | S_while Expr Stmt | S_if Expr Stmt | S_seq Stmt Stmt | S_obs Expr
          | S_undef
          deriving (Show, Generic)

instance Arbitrary Stmt where
      shrink = genericShrink
      arbitrary = sized arb
            where arb n | n <= 1 = oneof
                        [ return $ S_obs (E_ide "x")
                        , return $ S_obs (E_ide "y")
                        , S_obs <$> arbitrary
                        , return S_undef
                        , S_assign "x" <$> arbitrary
                        , S_assign "y" <$> arbitrary
                        ]
                  arb n = oneof
                        [ S_if <$> arbitrary <*> arb (n - 1)
                        -- , S_while <$> arbitrary <*> arb (n - 1)
                        , S_seq <$> arb (n `div` 2) <*> arb (n `div` 2)
                        ]

ppStmt :: Stmt -> String
ppStmt = \ case
      S_assign x e      -> x ++ " = " ++ ppExpr e
      S_while e s       -> "while (" ++ ppExpr e ++ ") {\n" ++ ppStmt s ++ "\n}\n"
      S_if e s          -> "if (" ++ ppExpr e ++ ") {\n" ++ ppStmt s ++ "\n}\n"
      S_seq s1 s2       -> ppStmt s1 ++ ";\n" ++ ppStmt s2
      S_obs e           -> "obs " ++ ppExpr e
      S_undef           -> "undef"

undef :: String -> R Abs a
undef = Resume . lift . EitherT . return . Left

absExpr :: Expr -> R Abs N
absExpr (E_plus e1 e2) = do
      n1 <- absExpr e1
      n2 <- absExpr e2
      return (n1 + n2)
absExpr (E_minus e1 e2) = do
      n1 <- absExpr e1
      n2 <- absExpr e2
      return (n1 - n2)
absExpr (E_or e1 e2) = do
      n1 <- absExpr e1
      n2 <- absExpr e2
      return (if (n1 /= 0) || (n2 /= 0) then 1 else 0)
absExpr (E_and e1 e2) = do
      n1 <- absExpr e1
      n2 <- absExpr e2
      return (if (n1 /= 0) && (n2 /= 0) then 1 else 0)
absExpr (E_not e) = do
      n <- absExpr e
      return (if n /= 0 then 0 else 1)
absExpr (E_int n) = return n
absExpr (E_ide x) = do
      s <- get
      return (load' x s)
absExpr (E_choose e1 e2) = absExpr e1 +|+ absExpr e2

absStmt :: Stmt -> R Abs ()
absStmt (S_assign x e) = do
      n <- absExpr e
      modify (store' x n)
absStmt S_undef = undef "Undef stmt."
absStmt (S_obs e) = do
      n <- absExpr e
      modify (obs (show n))
absStmt (S_while e s) = absStmt (S_if e (S_seq s (S_while e s)))
absStmt (S_if e s) = do
      b <- absExpr e
      when (b /= 0) $ absStmt s
absStmt (S_seq s1 s2) = do
      absStmt s1
      absStmt s2

concExpr :: Expr -> R Conc N
concExpr (E_plus e1 e2) = do
      n1 <- concExpr e1
      n2 <- concExpr e2
      return (n1 + n2)
concExpr (E_minus e1 e2) = do
      n1 <- concExpr e1
      n2 <- concExpr e2
      return (n1 - n2)
concExpr (E_or e1 e2) = do
      n1 <- concExpr e1
      n2 <- concExpr e2
      return (if (n1 /= 0) || (n2 /= 0) then 1 else 0)
concExpr (E_and e1 e2) = do
      n1 <- concExpr e1
      n2 <- concExpr e2
      return (if (n1 /= 0) && (n2 /= 0) then 1 else 0)
concExpr (E_not e) = do
      n <- concExpr e
      return (if n /= 0 then 0 else 1)
concExpr (E_int n) = return n
concExpr (E_ide x) = do
      s <- get
      return (load' x s)
concExpr (E_choose e1 e2) = concExpr e1 +|+ concExpr e2

concStmt :: Stmt -> R Conc ()
concStmt (S_assign x e) = do
      n <- concExpr e
      modify (store' x n)
concStmt S_undef = return ()
concStmt (S_obs e) = do
      n <- concExpr e
      modify (obs (show n))
concStmt w@(S_while e s) = do
      b <- concExpr e
      when (b /= 0) $ concStmt (S_seq s w)
concStmt (S_if e s) = do
      b <- concExpr e
      when (b /= 0) $ concStmt s
concStmt (S_seq s1 s2) = do
      concStmt s1
      concStmt s2

abs = aproj . absStmt
conc = cproj . concStmt

ex1 :: Stmt
ex1 = S_seq
      (S_assign "x" (E_int 5))
      (S_while (E_ide "x") (S_seq (S_obs (E_ide "x")) (S_assign "x" (E_minus (E_ide "x") (E_int 1)))))

ex2 :: Stmt
ex2 = S_while (E_int 1) (S_obs (E_int 9))

p1 :: Stmt
p1 = S_seq (S_assign "n" (E_int 2))
      (S_seq (S_assign "x" (E_choose (E_int 0) (E_int 1)))
      (S_seq (S_obs (E_int 10))
      (S_while (E_ide "n")
            (S_seq (S_obs (E_ide "x"))
            (S_seq (S_if (E_not (E_ide "x")) S_undef)
            (S_assign "n" (E_minus (E_ide "n") (E_int 1))))))))

p2 :: Stmt
p2 = S_seq (S_assign "n" (E_int 2))
      (S_seq (S_assign "x" (E_choose (E_int 0) (E_int 1)))
      (S_seq (S_obs (E_int 10))
      (S_seq (S_if (E_not (E_ide "x")) S_undef)
      (S_while (E_ide "n")
            (S_seq (S_obs (E_ide "x"))
            (S_assign "n" (E_minus (E_ide "n") (E_int 1))))))))

p3 :: Stmt
p3 = S_seq (S_assign "n" (E_int 2))
      (S_seq (S_assign "x" (E_choose (E_int 0) (E_int 1)))
      (S_seq (S_if (E_not (E_ide "x")) S_undef)
      (S_seq (S_obs (E_int 10))
      (S_while (E_ide "n")
            (S_seq (S_obs (E_ide "x"))
            (S_assign "n" (E_minus (E_ide "n") (E_int 1))))))))

p4 :: Stmt
p4 = S_seq (S_assign "n" (E_int 2))
      (S_seq (S_assign "x" (E_choose (E_int 0) (E_int 1)))
      (S_seq (S_if (E_ide "x") S_undef)
      (S_seq (S_obs (E_int 10))
      (S_while (E_ide "x")
            (S_seq (S_obs (E_ide "x"))
            (S_assign "n" (E_minus (E_ide "n") (E_int 1))))))))


reg1 :: Stmt
reg1 = S_seq
      (S_if (E_choose (E_int 1) (E_not (E_int 1)))
            (S_obs (E_ide ""))
      )
      (S_seq (S_obs (E_ide ""))
      S_undef)
