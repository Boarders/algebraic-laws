{-# LANGUAGE TypeApplications #-}
module Monoid.Test where

import Monoid.Laws
import Data.Monoid
import Test.Tasty


testSuite :: TestTree
testSuite = testGroup ""
  [ anyLaws
  ]

anyLaws :: TestTree
anyLaws = monoidLaws @Any "Any"
