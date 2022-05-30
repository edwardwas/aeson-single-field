{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | A type for wrapping a single type in a record when encoding to and from JSON.
-- See `SingleField` for this type.
module Data.Aeson.SingleField (SingleField (..)) where

import Data.Aeson
import Data.Proxy (Proxy (..))
import GHC.Exts (fromString)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

-- | When interacting with or producing JSON, it is common to wrap a single field in
-- an object with a single field. This can be a bit awkward to use from Haskell - we
-- tend to write a single `ToJSON` and `FromJSON` instance for each type, making this
-- extra wrapping cumbersome.
--
-- This newtype wrapper can helper with this. @SingleField field a@ contains a single
-- value of @a@. The difference is that the `ToJSON` and `FromJSON` instances will wrap
-- the @a@ in an object with a single field @field@. @field@ is a /type level/ string,
-- using data kinds.
--
-- >>> encode (SingleField @"myField" 3)
-- "{\"myField\":3}"
--
-- >>> getSingleField <$> decode @(SingleField "myField" Int) (encode $ SingleField @"myField" 123)
-- Just 123
newtype SingleField (field :: Symbol) (a :: *) = SingleField {getSingleField :: a}
  deriving (Eq, Show, Functor)

keyFromProxy :: KnownSymbol n => Proxy n -> Key
keyFromProxy = fromString . symbolVal

instance (ToJSON a, KnownSymbol field) => ToJSON (SingleField field a) where
  toJSON (SingleField a) = object [keyFromProxy (Proxy @field) .= a]

instance (FromJSON a, KnownSymbol field) => FromJSON (SingleField field a) where
  parseJSON = withObject ("SingleField " <> symbolVal (Proxy @field)) $
    \obj -> SingleField <$> (obj .: keyFromProxy (Proxy @field))
