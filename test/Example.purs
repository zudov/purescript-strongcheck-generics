module StrongCheckExample (exampleMain) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Data.Generic (class Generic, gShow)

import Test.StrongCheck.Gen (Gen, showSample)
import Test.StrongCheck.Generic (gArbitrary)

data Tree a = Leaf | Branch { value :: a, kids :: Array (Tree a) }

derive instance genericTree :: (Generic a) => Generic (Tree a)

instance showTree :: (Show a, Generic a) => Show (Tree a) where
  show = gShow

exampleMain :: forall eff. Eff (console :: CONSOLE | eff) Unit
exampleMain = showSample (gArbitrary :: Gen (Tree Int))
