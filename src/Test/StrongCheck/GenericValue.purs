module Test.StrongCheck.GenericValue
  ( GenericValue()
  , genericValue
  , runGenericValue
  ) where

import Control.Bind (bind)
import Control.MonadZero (class MonadZero, guard)
import Data.Eq (class Eq)
import Data.Functor ((<$))
import Data.Generic (GenericSignature, GenericSpine, isValidSpine)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Test.StrongCheck.Arbitrary (class Arbitrary)
import Test.StrongCheck.Generic (genGenericSignature, genGenericSpine)

-- | Contains a representation of a value in generic form.
-- | Consists of `GenericSpine` and corresponding `GenericSignature`.
-- |
-- | Useful for its `Arbitary` instance.
newtype GenericValue =
  GV { signature :: GenericSignature
     , spine     :: GenericSpine
     }

runGenericValue
  :: GenericValue
  -> { signature :: GenericSignature
     , spine     :: GenericSpine
     }
runGenericValue (GV a) = a

derive instance eqGenericValue :: Eq GenericValue

instance showGenericValue :: Show GenericValue where
  show (GV { signature, spine }) =
    "genericValue " <> show signature
                    <> " "
                    <> show spine

instance arbitraryGenericValue :: Arbitrary GenericValue where
  arbitrary = do
    signature <- genGenericSignature
    spine     <- genGenericSpine signature
    genericValue signature spine

-- | Constructs generic value. Would return `mzero` if given
-- | `GenericSpine` doesn't conform to given `GenericSignature`.
genericValue
  :: ∀ m. MonadZero m
  => GenericSignature
  -> GenericSpine
  -> m GenericValue
genericValue signature spine =
  GV { signature, spine } <$ guard (isValidSpine signature spine)
