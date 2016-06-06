## Module Test.StrongCheck.Generic

Generic deriving for `Arbitrary` and `CoArbitrary` instances.
Generation of arbitrary `GenericSpine`s with corresponding `GenericSignature`s.

#### `gArbitrary`

``` purescript
gArbitrary :: forall a. Generic a => Gen a
```

Generate arbitrary values for any `Generic` data structure

#### `gCoarbitrary`

``` purescript
gCoarbitrary :: forall a r. Generic a => a -> Gen r -> Gen r
```

Perturb a generator using a `Generic` data structure

#### `GenericValue`

``` purescript
newtype GenericValue
```

Contains representation of an arbitrary value.
Consists of `GenericSpine` and corresponding `GenericSignature`.

##### Instances
``` purescript
Arbitrary GenericValue
```

#### `runGenericValue`

``` purescript
runGenericValue :: GenericValue -> { signature :: GenericSignature, spine :: GenericSpine }
```

Extract `GenericSignature` and `GenericSpine` from a `GenericValue`

#### `genericValue`

``` purescript
genericValue :: GenericSignature -> GenericSpine -> Maybe GenericValue
```

Smart constructor for `GenericValue`. Would return `Nothing` if given
`GenericSpine` doesn't conform to given `GenericSignature`

#### `genGenericSignature`

``` purescript
genGenericSignature :: Size -> Gen GenericSignature
```

Generates `GenericSignature`s. Size parameter affects how nested the structure is.

#### `genGenericSpine`

``` purescript
genGenericSpine :: GenericSignature -> Gen GenericSpine
```

Generates `GenericSpine`s that conform to provided `GenericSignature`.


