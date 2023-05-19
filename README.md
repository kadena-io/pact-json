# Pact JSON Encoding

This library provides serialization functions suitable for use with Pact
constructs on the Kadena blockchain.

Its focus is on precice and explicit control of serialization format by the
library user. It prioritizes these features over convenience; there is no
`Generics`-based instance deriving and no generic intermediate value like
`Data.Aeson.Value`.

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

example2 :: BS.ByteString
example2 = J.encode (J.object ["a" J..= J.Aeson (1::Int)])

example3 :: Text
example3 = J.encodeText (J.object ["a" J..= J.Aeson (1::Int)])
```
