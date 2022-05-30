Aeson Single Field
=================

When working with HTMX, I found the slightly irritating requirement that all JSON fragments must be records, not any other type of literals. This resulting in some boilerplate code to make newtype wrappers to wrap any type whose JSON forms where not records. This package exports a single type (`SingleField`) that helps reduce boilerplate for this problem.

This is a literate Haskell file, so we start by importing some packages and setting some language options

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

import Data.Aeson (FromJSON, ToJSON, encode)
import Data.Aeson.SingleField (SingleField(..))
import GHC.Generics (Generic())
import Test.HUnit
  ( Assertion
  , Test(TestCase, TestList)
  , assertEqual
  , runTestTTAndExit
  )
```

As an example, consider the following type.

```haskell
data MyPair =
  MyPair Int String
  deriving (Eq, Show, Generic)

instance ToJSON MyPair

instance FromJSON MyPair
```

This is a contrived example, but the JSON representation of `MyPair` is a vector.

```haskell
encodeMyPair :: Assertion
encodeMyPair =
  assertEqual "Encode instance" (encode $ MyPair 1 "hi") "[1,\"hi\"]"
```

We could override the `ToJSON` and `FromJSON` instances here, but that is error prone, and introduces extra complexity just to deal with a slight API mismatch. Instead we can do the following.

```haskell
encodeMyPairWithField :: Assertion
encodeMyPairWithField =
  let wrappedField :: SingleField "test-field" MyPair =
        SingleField $ MyPair 1 "hi"
   in assertEqual "Encode Instance" (encode wrappedField) "{\"test-field\":[1,\"hi\"]}"
```

Note the type level string - that is what gives us our field name. We can now see that our JSON is an object with a single field, just as we wanted. One can also write this in a more compact form with type applications.

```haskell
encodeMyPairWithFieldCompact :: Assertion
encodeMyPairWithFieldCompact =
  assertEqual
    "Encode Instance"
    (encode (SingleField @"test-field" (MyPair 1 "hi")))
    "{\"test-field\":[1,\"hi\"]}"
```

The following is required to run these assertions.

```haskell
main :: IO ()
main =
  runTestTTAndExit $
  TestList $
  map
    TestCase
    [encodeMyPair, encodeMyPairWithField, encodeMyPairWithFieldCompact]
```
