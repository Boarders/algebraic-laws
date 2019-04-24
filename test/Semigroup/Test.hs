{-# LANGUAGE TypeApplications #-}
module Semigroup.Test where

import Semigroup.Laws
import Data.Semigroup
import Test.Tasty


testSuite :: TestTree
testSuite = testGroup ""
  [ anyLaws
  ]


anyLaws :: TestTree
anyLaws = semigroupLaws @Any "Any"
