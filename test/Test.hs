module Main where

import qualified Semigroup.Test as Semigroup
import qualified Monoid.Test as Monoid
import Test.Tasty


main :: IO ()
main = defaultMain testSuite


testSuite :: TestTree
testSuite = testGroup "Algebraic Laws Test Suite:"
  [ Semigroup.testSuite
  , Monoid.testSuite
  ]
