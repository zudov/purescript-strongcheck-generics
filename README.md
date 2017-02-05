# purescript-strongcheck-generics

[![purescript-strongcheck-generics on Pursuit](http://pursuit.purescript.org/packages/purescript-strongcheck-generics/badge)](http://pursuit.purescript.org/packages/purescript-strongcheck-generics)
[![Latest release](http://img.shields.io/bower/v/purescript-strongcheck-generics.svg)](https://github.com/zudov/purescript-strongcheck-generics/releases)
[![Build Status](https://travis-ci.org/zudov/purescript-strongcheck-generics.svg?branch=master)](https://travis-ci.org/zudov/purescript-strongcheck-generics)

Generic deriving for `Arbitrary`/`CoArbitrary`, and generation of `GenericSpine`/`GenericSignature`.

**Warning:** It was noticed that generating a spine for deeply recursive data structures (particularly those
that include arrays of themselves) can hang running indefinetely. See [issue 5](https://github.com/zudov/purescript-strongcheck-generics/issues/5) for details.

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

[Output](https://travis-ci.org/zudov/purescript-strongcheck-generics/builds/91160401#L733)

## Documentation

- [Test.StrongCheck.Generic](docs/Test/StrongCheck/Generic.md)
