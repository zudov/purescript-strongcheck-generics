module Test.StrongCheck.Generic
  ( genGenericSignature
  , genGenericSpine
  , gArbitrary
  , GenericValue()
  , runGenericValue
  ) where

import Prelude

import Math
import Control.Lazy         (defer)
import Control.Plus         (empty)
import Control.Bind
import Data.Array           (nub, sortBy, uncons, zipWith, length, (!!))
import Data.Int             (toNumber)
import Data.Foldable        (all, find)
import Data.Generic
import qualified Data.List  as L
import Data.Maybe
import Data.Maybe.Unsafe    (fromJust)
import Data.Tuple
import Data.Traversable     (for, traverse)
import Test.StrongCheck     (Arbitrary, arbitrary)
import Test.StrongCheck.Gen

genGenericSignature :: Size -> Gen GenericSignature
genGenericSignature size | size > 5 = genGenericSignature 5
genGenericSignature 0 = elements SigNumber
                                 (L.toList [ SigInt, SigString, SigBoolean ])
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
genGenericSpine = genGenericSpine' empty

genGenericSpine' :: L.List String -> GenericSignature -> Gen GenericSpine
genGenericSpine' trail SigBoolean     = SBoolean <$> arbitrary
genGenericSpine' trail SigNumber      = SNumber  <$> arbitrary
genGenericSpine' trail SigInt         = SInt     <$> arbitrary
genGenericSpine' trail SigString      = SString  <$> arbitrary
genGenericSpine' trail (SigArray sig) = SArray   <$> arrayOf (const <$> genGenericSpine' trail (sig unit))
genGenericSpine' trail (SigProd sigs) = do
  alt =<< maybe empty (\cons -> frequency cons.head (L.toList cons.tail))
                      (uncons $ map (map pure) ctors)
  where
    ctors = sigs <#> (\sig -> Tuple (6.0 / (5.0 + toNumber (L.length $ L.filter ((==) sig.sigConstructor) trail))) sig)
    alt altSig = SProd altSig.sigConstructor
                   <$> traverse (map const <<< genGenericSpine' (L.(:) altSig.sigConstructor trail) <<< (unit #))
                                    altSig.sigValues
genGenericSpine' trail (SigRecord fieldSigs) =
  SRecord <$> for fieldSigs \field -> do val <- genGenericSpine' trail (field.recValue unit)
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
