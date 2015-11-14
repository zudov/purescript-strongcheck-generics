module StrongCheckExample (exampleMain) where

import Prelude

import Data.Generic

import Test.StrongCheck
import Test.StrongCheck.Gen
import Test.StrongCheck.Generic

data Tree a = Leaf | Branch { value :: a, kids :: Array (Tree a) }

derive instance genericTree :: (Generic a) => Generic (Tree a)

instance showTree :: (Show a, Generic a) => Show (Tree a) where
  show = gShow

exampleMain = do
  showSample (gArbitrary :: Gen (Tree Int))
