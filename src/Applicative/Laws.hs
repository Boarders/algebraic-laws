{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE FlexibleContexts    #-}

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
import Control.Applicative


-- |
-- This defines a 'TestTree' for the 'Functor' laws consisitng of 'identity'
-- 'composition' and 'zlzdLaw'.
applicativeLaws
  :: forall f a b c
  . ( Applicative f
    , Arbitrary (f a), Arbitrary (f b), Arbitrary (f c)
    , Arbitrary (f (Fun a b)), Arbitrary (f (Fun b c))
    , Arbitrary a
    , Eq (f a), Eq (f b), Eq (f c)
    , Show (f a), Show (f b), Show (f c)
    , Show a, Show b, Show c
    , Show (f (Fun b c)), Show (f (Fun a b))
    , Function a, Function b, Function c
    , Function (f a)
    , CoArbitrary a, Arbitrary b
    , CoArbitrary b, Arbitrary c
    , CoArbitrary c
    , CoArbitrary (f a)
    , Arbitrary (f a)
    )
  => String -- ^ Name of type
  -> TestTree
applicativeLaws typeName =
    testGroup ("Functor laws for " <> typeName)
      [ QC.testProperty identityTestName     (identity    @f @a)
      , QC.testProperty compositionTestName  (composition @f @a @b @c)
      , QC.testProperty homomorphismTestName (homomorphism @f @a @b)
      , QC.testProperty interchangeTestName  (interchange @f @a @b)
      , QC.testProperty liftA2LawTestName  (liftA2Law @f @a @b @c)
      , QC.testProperty zlztzgLawTestName  (zlztzgLaw @f @a @b)
      , QC.testProperty ztzgLawTestName  (ztzgLaw @f @a @b)
      , QC.testProperty zlztLawTestName  (zlztLaw @f @a @b)
      , QC.testProperty functorLawTestName  (functorLaw @f @a @b)
      ]
  where
    identityTestName :: TestName
    identityTestName = "Identity"

    compositionTestName :: TestName
    compositionTestName = "Composition"

    homomorphismTestName :: TestName
    homomorphismTestName = "Homomorphism"

    interchangeTestName :: TestName
    interchangeTestName = "Interchange"

    liftA2LawTestName :: TestName
    liftA2LawTestName = "liftA2 Law"

    zlztzgLawTestName :: TestName
    zlztzgLawTestName = "<*> Law"

    ztzgLawTestName :: TestName
    ztzgLawTestName = "*> Law"

    zlztLawTestName :: TestName
    zlztLawTestName = "<* Law"

    functorLawTestName :: TestName
    functorLawTestName = "Functor Law"



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
    (pure (.) <*> fbc <*> fab <*> fa) === ((fbc <*> (fab <*> fa)))
  where
    fab = applyFun <$> fab'
    fbc = applyFun <$> fbc'


-- |
--       @(<$) === fmap . const@
homomorphism
  :: forall f a b
  . ( Applicative f
    , Eq (f b), Show (f b)
    , Arbitrary a, CoArbitrary b
    , Function a, Function b
    )
  => Fun a b -> a -> Property
  
homomorphism fab' a = (===) @(f b) (pure fab <*> pure a) (pure (fab a))
  where
    fab = applyFun fab'


-- |
--       @u <*> pure y = pure ($ y) <*> u@
interchange
  :: forall f a b
  . ( Applicative f
    , Eq (f b), (Show (f b))
    , Arbitrary a, CoArbitrary b
    , Function a, Function b
    )
  => f (Fun a b) -> a -> Property
interchange fab' a =  (fab <*> pure a) === (pure ($ a) <*> fab)
  where
    fab = applyFun <$> fab'


-- |
--       @liftA2 f x y = f <$> x <*> y@
liftA2Law
  :: forall f a b c
  . ( Applicative f
    , Eq (f c), (Show (f c))
    , Function a, Function b, Function c
    , Arbitrary a, CoArbitrary b
    , Arbitrary b, CoArbitrary c
    )
  => Fun a (Fun b c) -> f a -> f b -> Property
liftA2Law fabc' fa fb = (liftA2 fabc fa fb) === (fabc <$> fa <*> fb)
  where
    fabc a = applyFun $ (applyFun fabc') a


-- |
--       @(<*>) === liftA2 id@

zlztzgLaw
  :: forall f a b
  . ( Applicative f
    , Eq (f b), Show (f b)
    , Function a, Function b
    , Arbitrary a, CoArbitrary b
    )
  => f (Fun a b) -> f a -> Property
zlztzgLaw fab' fa = (fab <*> fa) === (liftA2 id fab fa)
  where
    fab = applyFun <$> fab'


-- |
--       @u *> v = (id <$ u) <*> v@
ztzgLaw
  :: forall f a b
  . ( Applicative f
    , Eq (f b), Show (f b)
    , Arbitrary (f a), Arbitrary (f b)
    )
  => f a -> f b -> Property
ztzgLaw fa fb = (fa *> fb) === ((id <$ fa) <*> fb)


-- |
--       @u <* v = liftA2 const u v@
zlztLaw
  :: forall f a b
  . ( Applicative f
    , Eq (f a), Show (f a)
    , Arbitrary (f a), Arbitrary (f b)
    )
  => f a -> f b -> Property
zlztLaw fa fb = (fa <* fb) === (liftA2 const fa fb)


-- |
--       @fmap f x = pure f <*> x@
functorLaw
  :: forall f a b
  . ( Applicative f
    , Eq (f b), Show (f b)
    , Function a, Function b
    , Arbitrary a, CoArbitrary b
    )
  => Fun a b -> f a -> Property
functorLaw fab' fa = (fmap fab fa) === (pure fab <*> fa)
  where
    fab = applyFun fab'
  

