{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | This module provides property tests for the Eq laws:
--
-- * reflexivity
--
--       @x == x = True@
--
-- * symmetry
--
--       @x == y = y == x@
--
-- * transitivity
--
--       @if x == y && y == z = True then x == z = True@
--
-- * substitutivity
--
--       @x :: a, x == y = True, f :: Eq b => a -> b ⊢ f x == f y = True@
--
-- * negation
--
--       @x /= y = not (x == y)@




module Eq.Laws where

import Test.Tasty (TestTree, testGroup, TestName)
import Test.Tasty.QuickCheck  as QC


-- |
-- This defines a 'TestTree' for the 'Eq' laws consisitng of the 'reflexivity'
-- 'symmetry', transitivity', 'substitutivity' and 'negation' laws.
eqLaws
  :: forall a b
  . ( Arbitrary a
    , Eq a, Eq b
    , Show a, Show b
    , Function a, Function b
    , CoArbitrary a, Arbitrary b
    )
  => String -- ^ Name of type
  -> TestTree
eqLaws typeName =
    testGroup ("Eq laws for " <> typeName)
      [ QC.testProperty reflexivityTestName    (reflexivity @a      )
      , QC.testProperty symmetryTestName       (symmetry @a         )
      , QC.testProperty transitivityTestName   (transitivity @a     )
      , QC.testProperty substitutivityTestName (substitutivity @a @b)
      , QC.testProperty negationTestName       (reflexivity @a      )
      ]
  where
    reflexivityTestName :: TestName
    reflexivityTestName = "Reflexivity"
 
    symmetryTestName :: TestName
    symmetryTestName = "Symmetry"

    transitivityTestName :: TestName
    transitivityTestName = "Transitivity"

    substitutivityTestName :: TestName
    substitutivityTestName = "Substitutivity"

    negationTestName :: TestName
    negationTestName = "Negation"
    

-- |
--       @x == x = True@
reflexivity :: forall a . (Eq a, Show a) => a -> Property
reflexivity x = (x == x) === True


-- |
--       @x == y = y == x@
symmetry :: forall a . (Eq a, Show a) => a -> a -> Property
symmetry x y = (x == y) === (y == x)


-- |
--       @if x == y && y == z = True then x == z = True@
transitivity :: forall a . (Eq a, Show a) => a -> a -> a -> Property
transitivity x y z
  | x == y  && y == z = (x == z) === True
  | otherwise = property True


-- |
--       @x :: a, x == y = True, f :: Eq b => a -> b ⊢ f x == f y = Tru@e
substitutivity
  :: forall a b
  . ( Eq a, Eq b
    , Show a
    , Function a, Function b
    , CoArbitrary a, Arbitrary b
    )
  => a -> a -> Fun a b -> Property
substitutivity x y f
  |  x == y = (applyFun f x == applyFun f y) === True
  | otherwise = property True

  

-- |
--       @x /= y = not (x == y)@
negation :: forall a . (Eq a, Show a) => a -> a -> Property
negation x y = (x /= y) === (not (x == y))



