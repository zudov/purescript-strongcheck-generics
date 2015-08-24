module Test.Main where

import Prelude

import Control.Monad.Trampoline
import Control.Monad.Eff.Console
import Data.Generic
import Data.Array (null)

import Test.StrongCheck
import Test.StrongCheck.Gen
import Test.StrongCheck.Generic

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

instance showTree :: (Generic a) => Show (Tree a) where
  show = gShow

props_gArbitrary = do
  quickCheck prop_arbitrary_foo_is_foo
  assert assert_uninhabited
  showSample (gArbitrary :: Gen (MyList Int))
  showSample (resize 5 $ gArbitrary :: Gen (Tree Int))

main = do
  props_gArbitrary
