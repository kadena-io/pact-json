# Pact JSON Encoding

This library provides serialization functions suitable for use with Pact
constructs on the Kadena blockchain.

Its focus is on precice and explicit control of serialization format by the
library user. It prioritizes these features over convenience; there is no
`Generics`-based instance deriving and no generic intermediate value like
`Data.Aeson.Value`.
The library includes a number of utils to reproduce encodings that are backward compatible with `aeson <2`, `hashable <1.3.1`, and `unordered-containers <0.2.16`. These utils can be found under `Pact.JSON.Legacy`.

## Exmaple usage

Encoding various values to JSON:

``` haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Pact.JSON.Encode as J

example1 :: BS.ByteString
example1 = J.encodeStrict (J.object ["a" J..= J.Aeson (1::Int)])
    -- the 'J.Aeson' combinator provides encodings for primitive types that are compatible with `aeson`.

example2 :: BS.ByteString
example2 = J.encode (J.object ["a" J..= J.Aeson (1::Int)])

example3 :: Text
example3 = J.encodeText (J.object ["a" J..= J.Aeson (1::Int)])
```
