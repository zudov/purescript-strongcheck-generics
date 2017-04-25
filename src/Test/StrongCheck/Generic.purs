-- | Generic deriving for `Arbitrary` and `CoArbitrary` instances.
-- | Generation of arbitrary `GenericSpine`s with corresponding `GenericSignature`s.
module Test.StrongCheck.Generic
  ( gArbitrary
  , gCoarbitrary
  , GenericValue
  , genericValue
  , runGenericValue
  , genGenericSignature
  , genGenericSpine
  ) where

import Prelude
import Control.Plus (empty)
import Data.Array (filter, length, nub, uncons, zipWith, (:))
import Data.Foldable (class Foldable, fold, foldMap)
import Data.Generic
  ( class Generic, GenericSignature(..), GenericSpine(..), DataConstructor
  , fromSpine, isValidSpine, toSignature, toSpine
  )
import Data.Int (toNumber)
import Data.List (fromFoldable)
import Data.Maybe (Maybe(Nothing, Just), fromJust, maybe)
import Data.Monoid (mempty)
import Data.Monoid.Endo (Endo(..))
import Data.Profunctor.Strong (second)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Test.StrongCheck.Arbitrary
  ( class Arbitrary, arbitrary, coarbitrary )
import Test.StrongCheck.Gen
  ( Gen, Size, arrayOf, elements, frequency, oneOf, resize, sized )
import Type.Proxy (Proxy(..))

-- | Generate arbitrary values for any `Generic` data structure
gArbitrary :: forall a. Generic a => Gen a
gArbitrary = unsafePartial fromJust <<< fromSpine <$> genGenericSpine (toSignature (Proxy :: Proxy a))

-- | Perturb a generator using a `Generic` data structure
gCoarbitrary :: forall a r. Generic a => a -> Gen r -> Gen r
gCoarbitrary = go <<< toSpine
  where
    go :: GenericSpine -> Gen r -> Gen r
    go (SArray ss)     = applyAll (map (go <<< (_ $ unit)) ss)
    go (SBoolean b)    = coarbitrary b
    go (SString s)     = coarbitrary s
    go (SChar c)       = coarbitrary c
    go (SInt i)        = coarbitrary i
    go (SNumber n)     = coarbitrary n
    go (SRecord fs)    = applyAll (map (\f -> coarbitrary f.recLabel <<< go (f.recValue unit)) fs)
    go (SProd ctor ss) = coarbitrary ctor <<< applyAll (map (go <<< force) ss)
    go SUnit           = coarbitrary unit

applyAll :: forall f a. Foldable f => f (a -> a) -> a -> a
applyAll = (case _ of Endo a -> a) <<< foldMap Endo

-- | Contains representation of an arbitrary value.
-- | Consists of `GenericSpine` and corresponding `GenericSignature`.
newtype GenericValue = GenericValue { signature :: GenericSignature
                                    , spine     :: GenericSpine
                                    }

-- | Extract `GenericSignature` and `GenericSpine` from a `GenericValue`
runGenericValue :: GenericValue -> { signature :: GenericSignature
                                   , spine     :: GenericSpine
                                   }
runGenericValue (GenericValue val) = val

-- | Smart constructor for `GenericValue`. Would return `Nothing` if given
-- | `GenericSpine` doesn't conform to given `GenericSignature`
genericValue :: GenericSignature -> GenericSpine -> Maybe GenericValue
genericValue signature spine
  | isValidSpine signature spine = Just $ GenericValue { signature, spine }
  | otherwise = Nothing

instance arbitraryGenericValue :: Arbitrary GenericValue where
  arbitrary = do
    signature <- sized genGenericSignature
    spine <- genGenericSpine signature
    maybe empty pure $ genericValue signature spine

-- | Generates `GenericSignature`s. Size parameter affects how nested the structure is.
genGenericSignature :: Size -> Gen GenericSignature
genGenericSignature size
  | size > 5 = genGenericSignature 5
  | size == 0 =
      elements SigNumber $ fromFoldable [ SigInt, SigString, SigBoolean ]
  | otherwise = resize (size - 1) $ oneOf sigArray [ sigProd, sigRecord ]
  where
    sigArray = SigArray <<< defer <$> sized genGenericSignature
    sigRecord = do
      labels <- nub <$> arrayOf arbitrary
      values <- arrayOf (defer <$> sized genGenericSignature)
      let fields = zipWith { recLabel: _, recValue: _ } labels values
      pure $ SigRecord fields
    sigProd = do
      typeConstructor <- arbitrary
      constructors <- nub <$> arrayOf arbitrary
      values  <- arrayOf (arrayOf (const <$> sized genGenericSignature))
      let dataConstructors = zipWith { sigConstructor: _, sigValues: _ }
                               constructors values
      pure $ SigProd typeConstructor dataConstructors

-- | Generates `GenericSpine`s that conform to provided `GenericSignature`.
genGenericSpine :: GenericSignature -> Gen GenericSpine
genGenericSpine = map force <<< genGenericSpine' mempty

genGenericSpine' :: Array String -> GenericSignature -> Gen (Unit -> GenericSpine)
genGenericSpine' trail SigBoolean     = defer <<< SBoolean <$> arbitrary
genGenericSpine' trail SigNumber      = defer <<< SNumber  <$> arbitrary
genGenericSpine' trail SigInt         = defer <<< SInt     <$> arbitrary
genGenericSpine' trail SigString      = defer <<< SString  <$> arbitrary
genGenericSpine' trail SigChar        = defer <<< SChar    <$> arbitrary
genGenericSpine' trail SigUnit        = pure $ defer SUnit
genGenericSpine' trail (SigArray sig) =
  defer <<< SArray <$> arrayOf (genGenericSpine' trail (force sig))
genGenericSpine' trail (SigProd _ constructors) = defer <$> do
  { sigConstructor, sigValues } <- fold (frequencyWith probability <$> uncons constructors)
  spines <- for sigValues (force >>> genGenericSpine' (sigConstructor : trail))
  pure $ SProd sigConstructor spines
  where
    probability a = 6.0 / (5.0 + toNumber (trailCount a))
      where
        -- | How many times the given `DataConstructor` has already been used
        trailCount :: DataConstructor -> Int
        trailCount { sigConstructor } = length $ filter (eq sigConstructor) trail
genGenericSpine' trail (SigRecord fieldSigs) =
  defer <<< SRecord <$> for fieldSigs \fieldSig -> do
    val <- genGenericSpine' trail $ force fieldSig.recValue
    pure $ fieldSig { recValue = val }

frequencyWith :: forall a f
               . Functor f
              => Foldable f
              => (a -> Number)
              -> { head :: a, tail :: f a }
              -> Gen a
frequencyWith probabilityFn { head, tail } =
  frequency
    (second pure $ annotate probabilityFn head)
    (fromFoldable $ map (second pure <<< annotate probabilityFn) tail)

annotate :: forall a b. (a -> b) -> a -> Tuple b a
annotate f a = Tuple (f a) a

defer :: forall a b. a -> (b -> a)
defer = const

force :: forall a. (Unit -> a) -> a
force a = a unit
