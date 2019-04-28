{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | This module provides property tests for the semigroup laws:
--
-- * associativity
--
--       @x <> (y <> z) = (x <> y) <> z@
module Semigroup.Laws where

import           Test.Tasty            (TestName, TestTree, testGroup)
import           Test.Tasty.QuickCheck as QC

-- |
-- This defines a 'TestTree' for the 'Semigroup' laws consisitng of the 'associativity'
-- property.
semigroupLaws
  :: forall a . (Semigroup a, Arbitrary a, Eq a, Show a)
  => String -- ^ Name of type
  -> TestTree
semigroupLaws typeName =
    testGroup ("Semigroup laws for " <> typeName)
      [ QC.testProperty associativityTestName (associativity @a)
      ]
  where
    associativityTestName :: TestName
    associativityTestName = "Associativity"

-- |
--       @x <> (y <> z) = (x <> y) <> z@
associativity :: forall a . (Semigroup a, Eq a, Show a) => a -> a -> a -> Property
associativity x y z = (x <> (y <> z)) === ((x <> y) <> z)
