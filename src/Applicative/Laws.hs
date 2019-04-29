{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | This module provides property tests for the functor laws:
--
-- * identity
--
--       @pure id <*> v = v@
--
-- * composition
--
--       @pure (.) <*> u <*> v <*> w = u <*> (v <*> w)@
--
-- * homomorphism
--
--       @pure f <*> pure x = pure (f x)@
--
-- * interchange
--
--       @u <*> pure y = pure ($ y) <*> u@
--
-- * liftA2 law
--
--       @liftA2 f x y = f <$> x <*> y@
--
-- * <*> law
--
--       @(<*>) === liftA2 id@
--
-- * *> law
--
--       @u *> v = (id <$ u) <*> v@
--
-- * <* law
--
--       @u <* v = liftA2 const u v@
--
-- * functor law
--
--       @fmap f x = pure f <*> x@
module Applicative.Laws where

import           Test.Tasty            (TestName, TestTree, testGroup)
import           Test.Tasty.QuickCheck as QC


-- |
-- This defines a 'TestTree' for the 'Functor' laws consisitng of 'identity'
-- 'composition' and 'zlzdLaw'.
applicativeLaws
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
applicativeLaws typeName =
    testGroup ("Functor laws for " <> typeName)
      [-- QC.testProperty identityTestName    (identity    @f @a)
      ]
  where
    identityTestName :: TestName
    identityTestName = "Identity"

    compositionTestName :: TestName
    compositionTestName = "Composition"

    zlzdLawTestName :: TestName
    zlzdLawTestName = "<$ Law"


-- |
--       @pure id <*> v = v@
identity :: forall f a . (Applicative f, Eq (f a), Show (f a)) => f a -> Property
identity fa = (pure id <*> fa) === fa

-- |
--       @pure (.) <*> u <*> v <*> w = u <*> (v <*> w)@
composition
  :: forall f a b c
  . ( Applicative f
    , Arbitrary (f (Fun a b)), Arbitrary (f (Fun b c))
    , Arbitrary (f a)
    , Eq (f c)
    , Show (f c)
    )
  => f (Fun b c) -> f (Fun a b) -> f a -> Property
composition fbc' fab' fa =
    (pure (.) <*> fbc <*> fab <*> fa) === (fbc <*> (fab <*> fa))
  where
    fab = applyFun <$> fab'
    fbc = applyFun <$> fbc'
--
-- * homomorphism
--
--       @(<$) === fmap . const@
homomorphism
  :: forall a b f
  . ( Applicative f
    , Eq (f b), Show (f b)
    , Arbitrary a
    , Arbitrary a, CoArbitrary b
    , Function a, Function b
    )
  => Fun a b -> a -> Property
  
homomorphism f' a = (===) @(f b) (pure f <*> pure a) (pure (f a))
  where
    f = applyFun f'
--
-- * interchange
--
--       @u <*> pure y = pure ($ y) <*> u@
--
-- * liftA2 law
--
--       @liftA2 f x y = f <$> x <*> y@
--
-- * <*> law
--
--       @(<*>) === liftA2 id@
--
-- * *> law
--
--       @u *> v = (id <$ u) <*> v@
--
-- * <* law
--
--       @u <* v = liftA2 const u v@
--
-- * functor law
--
--       @fmap f x = pure f <*> x@

-- |
--       @fmap id === id@






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
