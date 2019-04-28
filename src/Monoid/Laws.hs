{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | This module provides property tests for the monoid laws:
--
-- * associativity
--
--       @x <> (y <> z) = (x <> y) <> z@
--
-- * left unit
--
--       @mempty <> x = x@
--
-- * right unit
--
--       @x <> mempty = x@
--
-- * mconcat law
--
--       @mconcat xs = foldr (<>) mempty xs@

module Monoid.Laws where

import           Semigroup.Laws
import           Test.Tasty            (TestName, TestTree, testGroup)
import           Test.Tasty.QuickCheck as QC


-- | This defines a 'TestTree' for the 'Monoid' laws consisting of 'associativity',
--   'leftUnit', 'rightUnit' and the 'mconcatLaw'.
monoidLaws
  :: forall a . (Monoid a, Arbitrary a, Eq a, Show a)
  => String -- ^ Name of type
  -> TestTree
monoidLaws typeName = testGroup ("Monoid laws for " <> typeName)
    [ semigroupLaws @a typeName
    , QC.testProperty leftIdentityTestName  (leftUnit   @a)
    , QC.testProperty rightIdentityTestName (rightUnit  @a)
    , QC.testProperty mconcatLawTestName    (mconcatLaw @a)
    ]
  where
  leftIdentityTestName :: TestName
  leftIdentityTestName = "Left unit"

  rightIdentityTestName :: TestName
  rightIdentityTestName = "Right unit"

  mconcatLawTestName :: TestName
  mconcatLawTestName = "mconcat xs = foldr (<>) mempty"


-- |
--       @mempty <> x = x@
leftUnit :: forall a . (Monoid a, Arbitrary a, Eq a, Show a)
  => a -> Property
leftUnit x = mempty <> x === x


-- |
--      @x <> mempty = x@
rightUnit :: forall a . (Monoid a, Arbitrary a, Eq a, Show a) =>
  a -> Property
rightUnit x = x <> mempty === x


-- |
--       @mconcat xs = foldr (<>) mempty xs@
mconcatLaw :: forall a . (Monoid a, Arbitrary a, Eq a, Show a) =>
  [a] -> Property
mconcatLaw xs =
      mconcat xs === (foldr (<>) mempty) xs
