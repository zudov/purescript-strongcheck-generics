module Test.Main where

import Prelude

import Control.Monad.Trampoline
import Control.Monad.Eff.Console
import Data.Generic
import Data.Array (null)
import Data.Foldable (any)

import Test.StrongCheck
import Test.StrongCheck.Gen
import Test.StrongCheck.Generic

import StrongCheckExample

data Foo = Foo
derive instance genericFoo :: Generic Foo

instance arbitraryFoo :: Arbitrary Foo where
  arbitrary = gArbitrary

-- | A very dumb property
prop_arbitrary_foo_is_foo :: Foo -> Boolean
prop_arbitrary_foo_is_foo Foo = true

data Uninhabited
derive instance genericUninhabited :: Generic Uninhabited

-- | Check that `gArbitrary :: Gen Uninhabited` results into an empty generator
assert_uninhabited :: Boolean
assert_uninhabited = null $ runTrampoline $ collectAll state (gArbitrary :: Gen Uninhabited)
  where state = GenState { seed: 42.0, size: 42 }

data MyList a = Nil | Cons (MyList a)
derive instance genericMyList :: (Generic a) => Generic (MyList a)

instance showMyList :: (Generic a) => Show (MyList a) where
  show = gShow

data Tree a = Leaf | Branch { value :: a, kids :: Array (Tree a) }
derive instance genericTree :: (Generic a) => Generic (Tree a)

anywhere :: forall a. (a -> Boolean) -> Tree a -> Boolean
anywhere _ Leaf = false
anywhere p (Branch o) = p o.value || any (anywhere p) o.kids

instance showTree :: (Generic a) => Show (Tree a) where
  show = gShow

instance arbitraryTree :: (Generic a) => Arbitrary (Tree a) where
  arbitrary = gArbitrary

instance coarbitraryTree :: (Generic a) => CoArbitrary (Tree a) where
  coarbitrary = gCoarbitrary

props_gArbitrary = do
  quickCheck prop_arbitrary_foo_is_foo
  assert assert_uninhabited
  showSample (gArbitrary :: Gen (MyList Int))
  showSample (gArbitrary :: Gen (Tree Int))
  quickCheck $ \f g t -> anywhere f (t :: Tree Int) || anywhere g t == anywhere (\a -> f a || g a) t

main = do
  props_gArbitrary
  exampleMain
