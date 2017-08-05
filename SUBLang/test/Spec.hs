{-# LANGUAGE MultiParamTypeClasses
      , FlexibleInstances
      , ScopedTypeVariables
      #-}

module Main where

import SUBLang
import qualified UBExp as E
import qualified UBStmt as S
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

main = defaultMain tests

tests :: TestTree
tests = testGroup " Tests" [qcProps, unitTests]

qcProps :: TestTree
qcProps
      = localOption (QuickCheckTests 250)
      $ localOption (QuickCheckMaxSize 14)
      $ localOption (QuickCheckVerbose False)
      $ localOption (QuickCheckShowReplay True)
      $ localOption (Timeout 10000000 "10s")
      $ testGroup "QuickCheck:"
      [ QC.testProperty "forall e. E.conc e <: prune_minus (E.abs e)"
            $ \ e -> E.conc e <: prune_minus (E.abs e)
      , QC.testProperty "forall p. S.conc p <: prune_minus (S.abs p)"
            $ \ p -> S.conc p <: prune_minus (S.abs p)
      ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
      [ testCase "UBExp: conc ub1 <: prune (abs ub1)" $
            (E.conc E.ub1 <: prune_minus (E.abs E.ub1)) @?= True
      , testCase "UBExp: conc ub2 <: prune (abs ub2)" $
            (E.conc E.ub2 <: prune_minus (E.abs E.ub2)) @?= True
      , testCase "UBStmt: abs p1 =/= abs p2" $
            (S.abs S.p1 /= S.abs S.p2) @?= True
      , testCase "UBStmt: prune (abs p1) == prune (abs p2)" $
            (prune_minus (S.abs S.p1) == prune_minus (S.abs S.p2)) @?= True
      , testCase "UBStmt: prune (abs p1) =/= prune (abs p3)" $
            (prune_minus (S.abs S.p1) /= prune_minus (S.abs S.p3)) @?= True
      , testCase "UBStmt: conc reg1 <: prune (abs reg1)" $
            (S.conc S.reg1 <: prune_minus (S.abs S.reg1)) @?= True
      ]

