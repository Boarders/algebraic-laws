{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | This module provides property tests for the functor laws:
--
-- * identity
--
--       @fmap id === id@
--
-- * composition
--
--       @fmap (f . g) === fmap f . fmap g@
--
-- * <$ law
--
--       @(<$) === fmap . const@
module Functor.Laws where

import           Test.Tasty            (TestName, TestTree, testGroup)
import           Test.Tasty.QuickCheck as QC


-- |
-- This defines a 'TestTree' for the 'Functor' laws consisitng of 'identity'
-- 'composition' and 'zlzdLaw'.
functorLaws
  :: forall f a b c
  . ( Functor f
    , Arbitrary (f a), Arbitrary (f b), Arbitrary (f c)
    , Arbitrary a
    , Eq (f a), Eq (f c)
    , Show (f a), Show (f b), Show (f c)
    , Show a, Show b, Show c
    , Function a, Function b
    , CoArbitrary a, Arbitrary b
    , CoArbitrary b, Arbitrary c
    )
  => String -- ^ Name of type
  -> TestTree
functorLaws typeName =
    testGroup ("Functor laws for " <> typeName)
      [ QC.testProperty identityTestName    (identity    @f @a)
      , QC.testProperty compositionTestName (composition @f @a @b @c)
      , QC.testProperty zlzdLawTestName     (zlzdLaw     @f @a @b)
      ]
  where
    identityTestName :: TestName
    identityTestName = "Identity"

    compositionTestName :: TestName
    compositionTestName = "Composition"

    zlzdLawTestName :: TestName
    zlzdLawTestName = "<$ Law"



-- |
--       @fmap id === id@
identity :: forall f a . (Functor f, Eq (f a), Show (f a)) => f a -> Property
identity fa = ((fmap @f) (id @a)) fa === (id @(f a)) fa


-- |
--       @fmap (f . g) == fmap f . fmap g@
composition
  :: forall f a b c
  . ( Functor f
    , Eq (f c)
    , Show (f c)
    , Show a, Show b, Show c
    , Function a, Function b
    , CoArbitrary a, Arbitrary b
    , CoArbitrary b, Arbitrary c
    )
  => (f a) -> Fun b c -> Fun a b  -> Property
composition fa g h =
      ((fmap @ f) (applyFun g . applyFun h)) fa
  === ((fmap @f) (applyFun g) . (fmap @f) (applyFun h)) fa



-- |
--       @(<$) === fmap . const@
zlzdLaw
  :: forall f a b
  . ( Functor f
    , Eq (f a)
    , Show (f a)
    , Arbitrary a, Arbitrary (f a)
    )
   => a -> f b -> Property
zlzdLaw a fb =
  (<$) a fb === (fmap . const) a fb
