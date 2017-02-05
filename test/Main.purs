module Test.Main where

import Prelude

import Control.Monad.Trampoline (runTrampoline)

import Data.Array (null)
import Data.Foldable (any)
import Data.Generic (class Generic, gShow)

import Partial.Unsafe (unsafePartial)

import Test.StrongCheck (SC, quickCheck, assert)
import Test.StrongCheck.Arbitrary (class Coarbitrary, class Arbitrary)
import Test.StrongCheck.Gen (Gen, GenState(..), showSample, collectAll, showSample')
import Test.StrongCheck.LCG (mkSeed)
import Test.StrongCheck.Generic (gArbitrary, gCoarbitrary)

--import StrongCheckExample (exampleMain)

data Foo = Foo
derive instance genericFoo :: Generic Foo

instance arbitraryFoo :: Arbitrary Foo where
  arbitrary = gArbitrary

-- | A very dumb property
prop_arbitrary_foo_is_foo :: Foo -> Boolean
prop_arbitrary_foo_is_foo Foo = true

data Uninhabited
derive instance genericUninhabited :: Partial => Generic Uninhabited

-- | Check that `gArbitrary :: Gen Uninhabited` results into an empty generator
assert_uninhabited :: Boolean
assert_uninhabited = unsafePartial $ null $ runTrampoline $ collectAll state (gArbitrary :: Gen Uninhabited)
  where state = GenState { seed: mkSeed 42, size: 42 }

data MyList a = Nil | Cons a (MyList a)
derive instance genericMyList :: Generic a => Generic (MyList a)

instance showMyList :: Generic a => Show (MyList a) where
  show = gShow

data Tree a = Leaf | Branch { value :: a, kids :: Array (Tree a) }
derive instance genericTree :: Generic a => Generic (Tree a)

anywhere :: forall a. (a -> Boolean) -> Tree a -> Boolean
anywhere _ Leaf = false
anywhere p (Branch o) = p o.value || any (anywhere p) o.kids

instance showTree :: Generic a => Show (Tree a) where
  show = gShow

instance arbitraryTree :: Generic a => Arbitrary (Tree a) where
  arbitrary = gArbitrary

instance coarbitraryTree :: Generic a => Coarbitrary (Tree a) where
  coarbitrary = gCoarbitrary

props_gArbitrary :: SC () Unit
props_gArbitrary = do
  quickCheck prop_arbitrary_foo_is_foo
  assert assert_uninhabited
  showSample' 1 (gArbitrary :: Gen (MyList Int))
--  showSample' 1 (gArbitrary :: Gen (Tree Int))
--  quickCheck $ \f g t -> anywhere f (t :: Tree Int) || anywhere g t == anywhere (\a -> f a || g a) t

main :: SC () Unit
main = do
  props_gArbitrary
--  exampleMain
