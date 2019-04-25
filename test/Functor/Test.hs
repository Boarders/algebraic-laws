{-# LANGUAGE TypeApplications #-}
module Functor.Test where

import Functor.Laws
import Test.Tasty


testSuite :: TestTree
testSuite = testGroup ""
  [ maybeFunctorLaws
  ]

maybeFunctorLaws :: TestTree
maybeFunctorLaws = functorLaws @Maybe @() @() @() "f = Maybe; a, b, c = ()"
