module Test.StrongCheck.Generic
  ( genGenericSignature
  , genGenericSpine
  , gArbitrary
  , GenericValue()
  , runGenericValue
  ) where

import Prelude

import Control.Lazy         (defer)
import Control.Plus         (empty)
import Control.Bind
import Data.Array           (nub, sortBy, uncons, zipWith, length, (!!))
import Data.Int             (toNumber)
import Data.Foldable        (all, find)
import Data.Generic
import Data.List            (toList)
import Data.Maybe
import Data.Maybe.Unsafe    (fromJust)
import Data.Tuple
import Data.Traversable     (for, traverse)
import Test.StrongCheck     (Arbitrary, arbitrary)
import Test.StrongCheck.Gen

genGenericSignature :: Size -> Gen GenericSignature
genGenericSignature size | size > 5 = genGenericSignature 5
genGenericSignature 0 = elements SigNumber
                                 (toList [ SigInt, SigString, SigBoolean ])
genGenericSignature size = resize (size - 1) $ oneOf sigArray [sigProd, sigRecord]
  where
    sigArray = SigArray <<< const <$> sized genGenericSignature
    sigRecord = do
      labels <- nub <$> arrayOf arbitrary
      values <- arrayOf (const <$> sized genGenericSignature)
      pure $ SigRecord $ zipWith { recLabel: _, recValue: _ } labels values
    sigProd = do
      constrs <- nub <$> arrayOf arbitrary
      values  <- arrayOf (arrayOf (const <$> sized genGenericSignature))
      pure $ SigProd $ zipWith { sigConstructor: _, sigValues: _ } constrs values

genGenericSpine :: GenericSignature -> Gen GenericSpine
genGenericSpine SigBoolean     = SBoolean <$> arbitrary
genGenericSpine SigNumber      = SNumber  <$> arbitrary
genGenericSpine SigInt         = SInt     <$> arbitrary
genGenericSpine SigString      = SString  <$> arbitrary
genGenericSpine (SigArray sig) = SArray   <$> arrayOf (const <$> genGenericSpine (sig unit))
genGenericSpine (SigProd sigs) =
  alt =<< maybe empty (\cons -> elements cons.head (toList cons.tail)) (uncons sigs)
  where
    alt altSig = SProd altSig.sigConstructor
                   <$> traverse (map const <<< genGenericSpine <<< (unit #))
                                    altSig.sigValues
genGenericSpine (SigRecord fieldSigs) =
  SRecord <$> for fieldSigs \field -> do val <- genGenericSpine (field.recValue unit)
                                         pure $ field { recValue = const val }

gArbitrary :: forall a. (Generic a) => Gen a
gArbitrary = fromJust <<< fromSpine <$> genGenericSpine (toSignature (Proxy :: Proxy a))

newtype GenericValue = GenericValue { signature :: GenericSignature
                                    , spine     :: GenericSpine
                                    }

runGenericValue :: GenericValue -> { signature :: GenericSignature
                                   , spine     :: GenericSpine
                                   }
runGenericValue (GenericValue val) = val

genericValue :: GenericSignature -> GenericSpine -> Maybe GenericValue
genericValue sig spine
  | isValidSpine sig spine = Just $ GenericValue {signature: sig, spine: spine}
  | otherwise = Nothing

instance arbitraryGenericValue :: Arbitrary GenericValue where
  arbitrary = do
    signature <- sized genGenericSignature
    spine <- genGenericSpine signature
    maybe empty pure $ genericValue signature spine
