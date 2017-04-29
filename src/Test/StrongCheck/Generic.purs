-- | Generic deriving for `Arbitrary` and `CoArbitrary` instances.
-- | Generation of arbitrary `GenericSpine`s with corresponding `GenericSignature`s.
module Test.StrongCheck.Generic
  ( gArbitrary
  , gCoarbitrary
  , genGenericSignature
  , genGenericSpine
  ) where

import Prelude
import Data.Array (filter, length, nub, uncons, zipWith, (:))
import Data.Foldable (class Foldable, fold, foldMap)
import Data.Generic (class Generic, GenericSignature(..), GenericSpine(..), fromSpine, toSignature, toSpine)
import Data.Int (toNumber)
import Data.List (fromFoldable)
import Data.Maybe (fromJust)
import Data.Monoid (mempty)
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (unwrap)
import Data.Profunctor.Strong (second)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Test.StrongCheck.Arbitrary (arbitrary, coarbitrary)
import Test.StrongCheck.Gen (Gen, arrayOf, elements, frequency, oneOf, resize, sized)
import Type.Proxy (Proxy(..))

-- | Generate arbitrary values for any `Generic` data structure
gArbitrary :: ∀ a. Generic a => Gen a
gArbitrary = unsafePartial fromJust <<< fromSpine <$> genGenericSpine (toSignature (Proxy :: Proxy a))

-- | Perturb a generator using a `Generic` data structure
gCoarbitrary :: ∀ a r. Generic a => a -> Gen r -> Gen r
gCoarbitrary = go <<< toSpine
  where
    go :: GenericSpine -> Gen r -> Gen r
    go (SArray ss)     = applyAll (map (go <<< force) ss)
    go (SBoolean b)    = coarbitrary b
    go (SString s)     = coarbitrary s
    go (SChar c)       = coarbitrary c
    go (SInt i)        = coarbitrary i
    go (SNumber n)     = coarbitrary n
    go (SRecord fs)    = applyAll (map (\f -> coarbitrary f.recLabel <<< go (force f.recValue)) fs)
    go (SProd ctor ss) = coarbitrary ctor <<< applyAll (map (go <<< force) ss)
    go SUnit           = coarbitrary unit

applyAll :: ∀ f a. Foldable f => f (a -> a) -> a -> a
applyAll = unwrap <<< foldMap Endo

-- | Generates `GenericSignature`s. Size parameter affects how nested the structure is.
genGenericSignature :: Gen GenericSignature
genGenericSignature = sized go
  where
    go 0 =
      elements SigNumber $ fromFoldable [ SigInt, SigString, SigBoolean ]
    go size =
      resize (min 5 size - 1) compound
      where
        recur = defer <$> sized go
        compound = oneOf sigArray [ sigProd, sigRecord ]
          where
            sigArray = SigArray <$> recur
            sigRecord = do
              labels <- nub <$> arrayOf arbitrary
              values <- arrayOf recur
              pure $ SigRecord
                   $ zipWith { recLabel: _, recValue: _ }
                       labels values
            sigProd = do
              type_   <- arbitrary
              constrs <- nub <$> arrayOf arbitrary
              values  <- arrayOf (arrayOf recur)
              pure $ SigProd type_
                   $ zipWith { sigConstructor: _, sigValues: _ }
                               constrs values

-- | Generates `GenericSpine`s that conform to provided `GenericSignature`.
genGenericSpine :: GenericSignature -> Gen GenericSpine
genGenericSpine = go mempty
  where
    go trail signature = case signature of
      SigUnit      -> pure SUnit
      SigBoolean   -> SBoolean <$> arbitrary
      SigInt       -> SInt     <$> arbitrary
      SigNumber    -> SNumber  <$> arbitrary
      SigChar      -> SChar    <$> arbitrary
      SigString    -> SString  <$> arbitrary
      SigArray  a  -> SArray   <$> arrayOf (defer <$> go trail (force a))
      SigRecord rs ->
        SRecord <$> for rs \r -> do
          value <- go trail $ force r.recValue
          pure $ r { recValue = defer value }
      SigProd _ dcs -> do
        dc   <- pick dcs
        vals <- for dc.sigValues
                  (force >>> go (dc.sigConstructor : trail))
        pure $ SProd dc.sigConstructor (defer <$> vals)
      where
        pick = -- pick one of the constructors
          fold <<< map (frequencyWith probability) <<< uncons
          where
            probability dc = -- probability of picking the constructor
              6.0 / (5.0 + toNumber (pickCount dc))
            pickCount dc = -- how many times the constructor was picked
              length $ filter (eq dc.sigConstructor) trail

frequencyWith
  :: ∀ a f. Functor f => Foldable f
  => (a -> Number)
  -> { head :: a, tail :: f a }
  -> Gen a
frequencyWith probabilityFn { head, tail } =
  frequency (probGen head) (fromFoldable $ probGen <$> tail)
  where
    probGen = second pure <<< annotate probabilityFn

annotate :: ∀ a b. (a -> b) -> a -> Tuple b a
annotate f a = Tuple (f a) a

defer :: ∀ a b. a -> (b -> a)
defer = const

force :: ∀ a. (Unit -> a) -> a
force a = a unit
