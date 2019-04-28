{-# LANGUAGE TypeApplications #-}
module Monoid.Test where

import           Data.Monoid
import           Monoid.Laws
import           Test.Tasty


testSuite :: TestTree
testSuite = testGroup ""
  [ anyLaws
  ]

anyLaws :: TestTree
anyLaws = monoidLaws @Any "Any"
