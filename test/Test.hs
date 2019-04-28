module Main where

import qualified Semigroup.Test as Semigroup
import qualified Monoid.Test as Monoid
import qualified Functor.Test as Functor
import qualified Eq.Test as Eq
import qualified Ord.Test as Ord
import Test.Tasty


main :: IO ()
main = defaultMain testSuite


testSuite :: TestTree
testSuite = testGroup "Algebraic Laws Test Suite:"
  [ Semigroup.testSuite
  , Monoid.testSuite
  , Functor.testSuite
  , Eq.testSuite
  , Ord.testSuite
  ]
