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
gArbitrary :: forall a. Generic a => Gen a
gArbitrary = unsafePartial fromJust <<< fromSpine <$> genGenericSpine (toSignature (Proxy :: Proxy a))

-- | Perturb a generator using a `Generic` data structure
gCoarbitrary :: forall a r. Generic a => a -> Gen r -> Gen r
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

applyAll :: forall f a. Foldable f => f (a -> a) -> a -> a
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
genGenericSpine = go mempty <<< defer
  where
    go trail signature = case force signature of
      SigUnit      -> pure SUnit
      SigBoolean   -> SBoolean <$> arbitrary
      SigInt       -> SInt     <$> arbitrary
      SigNumber    -> SNumber  <$> arbitrary
      SigChar      -> SChar    <$> arbitrary
      SigString    -> SString  <$> arbitrary
      SigArray  a  -> SArray   <$> arrayOf (defer <$> go trail a)
      SigRecord rs ->
        SRecord <$> for rs \r -> do
          value <- go trail r.recValue
          pure $ r { recValue = defer value }
      SigProd _ dcs -> do
        dc   <- pick dcs
        vals <- for dc.sigValues (go (breadcrumb dc) >>> map defer)
        pure $ SProd dc.sigConstructor vals
      where
        breadcrumb dc = dc.sigConstructor : trail

        -- pick one of the constructors
        pick =
          fold <<< map (frequencyWith likelihood) <<< uncons

        -- probability of picking the constructor
        likelihood dc =
          6.0 / (5.0 + toNumber (picked dc))

        -- how many times the constructor was picked
        picked dc =
          length $ filter (eq dc.sigConstructor) trail

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
