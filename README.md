# purescript-strongcheck-generics

[![purescript-strongcheck-generics on Pursuit](http://pursuit.purescript.org/packages/purescript-strongcheck-generics/badge)](http://pursuit.purescript.org/packages/purescript-strongcheck-generics)
[![Latest release](http://img.shields.io/bower/v/purescript-strongcheck-generics.svg)](https://github.com/zudov/purescript-strongcheck-generics/releases)
[![Build Status](https://travis-ci.org/zudov/purescript-strongcheck-generics.svg?branch=master)](https://travis-ci.org/zudov/purescript-strongcheck-generics)

Generic deriving for `Arbitrary`/`CoArbitrary`, and generation of `GenericSpine`/`GenericSignature`.

## Installation

```shell
bower install purescript-strongcheck-generics
```

## Example

```purescript
module Main where

import Prelude

import Data.Generic

import Test.StrongCheck
import Test.StrongCheck.Gen
import Test.StrongCheck.Generic

data Tree a = Leaf | Branch { value :: a, kids :: Array (Tree a) }

derive instance genericTree :: (Generic a) => Generic (Tree a)

instance showTree :: (Show a, Generic a) => Show (Tree a) where
  show = gShow

main = do
  showSample (gArbitrary :: Gen (Tree Int))
```

## Documentation

- [Test.StrongCheck.Generic](docs/Test/StrongCheck/Generic.md)
