{-# LANGUAGE TypeApplications #-}
module Eq.Test where

import           Eq.Laws
import           Test.Tasty


testSuite :: TestTree
testSuite = testGroup ""
  [ boolLaws
  ]


boolLaws :: TestTree
boolLaws = eqLaws @Bool @Int "Bool, Int"
