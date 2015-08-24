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

props_gArbitrary = do
  quickCheck prop_arbitrary_foo_is_foo
  assert assert_uninhabited

main = do
  props_gArbitrary
