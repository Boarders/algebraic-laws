{-# LANGUAGE TypeApplications #-}
module Ord.Test where

import Ord.Laws
import Test.Tasty


testSuite :: TestTree
testSuite = testGroup ""
  [ intLaws
  ]


intLaws :: TestTree
intLaws = ordLaws @Int "Int"
