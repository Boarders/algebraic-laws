{-# LANGUAGE TypeApplications #-}
module Semigroup.Test where

import           Data.Semigroup
import           Semigroup.Laws
import           Test.Tasty


testSuite :: TestTree
testSuite = testGroup ""
  [ anyLaws
  ]


anyLaws :: TestTree
anyLaws = semigroupLaws @Any "Any"
