{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | This module provides property tests for the Ord laws:
--
-- * transitivity
--
--       @if x <= y && y <= z = True, then x <= z = True@
--
-- * reflexivity
--
--       @x <= x = True@
--
-- * antisymmetry
--
--       @if x <= y && y <= x = True then x == y = True@
--
-- * >= law
--
--       @x >= y = y <= x@
--
-- * < law
--
--       @x < y = y < x@
--
-- * > law
--
--       @x > y = y < x@
--
-- * compare law
--
--       @x < y  = (compare x y == LT)@
--       @x == y = (compare x y == EQ)@
--       @x > y  = (compare x y == GT)@
--
-- * min law
--
--       @(min x y == if x <= y then x else y) = True@
--
-- * max law
--
--       @(max x y == if x >= y then x else y) = True@


module Ord.Laws where

import           Test.Tasty            (TestName, TestTree, testGroup)
import           Test.Tasty.QuickCheck as QC


-- |
-- This defines a 'TestTree' for the 'Ord' laws consisting of 'transitivty'
-- , 'reflexivity', 'antisymmetry', 'zgzeLaw', 'zlLaw', 'zgLaw', compareLaws'
-- , 'minLaw' and 'maxLaw'.
ordLaws
  :: forall a
  . ( Arbitrary a
    , Ord a
    , Show a
    )
  => String -- ^ Name of type
  -> TestTree
ordLaws typeName =
    testGroup ("Ord laws for " <> typeName)
      [ QC.testProperty transitivityTestName    (transitivity @a)
      , QC.testProperty reflexivityTestName    (reflexivity @a)
      , QC.testProperty antisymmetryTestName    (antisymmetry @a)
      , QC.testProperty zgzeLawTestName    (zgzeLaw @a)
      , QC.testProperty zlLawTestName    (zlLaw @a)
      , QC.testProperty zgLawTestName    (zgLaw @a)
      , QC.testProperty compareLaw1TestName    (compareLaw1 @a)
      , QC.testProperty compareLaw2TestName    (compareLaw2 @a)
      , QC.testProperty compareLaw3TestName    (compareLaw3 @a)
      , QC.testProperty minLawTestName    (minLaw @a)
      , QC.testProperty maxLawTestName    (maxLaw @a)
      ]
  where
    transitivityTestName :: TestName
    transitivityTestName = "Transitivity"

    reflexivityTestName :: TestName
    reflexivityTestName = "Reflexivity"

    antisymmetryTestName :: TestName
    antisymmetryTestName = "Antisymmetry"

    zgzeLawTestName :: TestName
    zgzeLawTestName = ">= Law"

    zlLawTestName :: TestName
    zlLawTestName = "< Law"

    zgLawTestName :: TestName
    zgLawTestName = "> Law"

    compareLaw1TestName :: TestName
    compareLaw1TestName = "Compare Law"

    compareLaw2TestName :: TestName
    compareLaw2TestName = "Compare Law"

    compareLaw3TestName :: TestName
    compareLaw3TestName = "Compare Law"

    minLawTestName :: TestName
    minLawTestName = "Min Law"

    maxLawTestName :: TestName
    maxLawTestName = "Max Law"


-- |
--       @if x <= y && y <= z = True, then x <= z = True@
transitivity :: forall a . (Show a, Ord a) => a -> a -> a -> Property
transitivity x y z
  | x <= y && y <= z = (x <= z) === True
  | otherwise        = property True

-- |
--       @x <= x = True@
reflexivity :: forall a. (Ord a, Show a) => a -> Property
reflexivity x = (x <= x) === True

-- |
--       @if x <= y && y <= x = True then x == y = True@
antisymmetry :: forall a . (Ord a, Show a) => a -> a -> Property
antisymmetry x y
  | x <= y && y <= x = (x == y) === True
  | otherwise        = property True


-- |
--       @x >= y = y <= x@
zgzeLaw :: forall a . (Show a, Ord a) => a -> a -> Property
zgzeLaw x y = (x >= y) === (y <= x)


-- |
--       @x < y = y < x@
zlLaw :: forall a . (Show a, Ord a) => a -> a -> Property
zlLaw x y = (x < y) === (y > x)

-- |
--       @x > y = y < x@
zgLaw :: forall a . (Show a, Ord a) => a -> a -> Property
zgLaw x y = (x > y) === (y < x)

-- |
--       @x < y  = (compare x y == LT)@
compareLaw1 :: forall a . (Show a, Ord a) => a -> a -> Property
compareLaw1 x y = (x < y) === (compare x y == LT)


-- |
--       @x == y = (compare x y == EQ)@
compareLaw2 :: forall a . (Show a, Ord a) => a -> a -> Property
compareLaw2 x y = (x == y) === (compare x y == EQ)

-- |
--       @x > y  = (compare x y == GT)@
compareLaw3 :: forall a . (Show a, Ord a) => a -> a -> Property
compareLaw3 x y = (x > y) === (compare x y == GT)




-- |
--       @(min x y == if x <= y then x else y) = True@
minLaw :: forall a . (Show a, Ord a) => a -> a -> Property
minLaw x y = (min x y) === (if x <= y then x else y)


-- |
--       @(max x y == if x >= y then x else y) = True@
maxLaw :: forall a . (Show a, Ord a) => a -> a -> Property
maxLaw x y = (max x y) === (if x >= y then x else y)
