{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Pact.JSON.Yaml
-- Copyright: Copyright © 2023 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Pact.JSON.Yaml
( encodeYaml
, encodeYamlWith
, encodeYamlFile
, encodeYamlFileWith
) where

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Yaml as Y

import GHC.Stack

import qualified Pact.JSON.Encode as J

-- -------------------------------------------------------------------------- --
-- YAML Utils

encodeYaml :: HasCallStack => J.Encode a => a -> B.ByteString
encodeYaml = Y.encode
  . either (error . show) id
  . Y.decodeEither' @Value
  . J.encodeStrict

encodeYamlWith
  :: HasCallStack
  => J.Encode a
  => Y.EncodeOptions
  -> a
  -> B.ByteString
encodeYamlWith opts = Y.encodeWith opts
  . either (error . show) id
  . Y.decodeEither' @Value
  . J.encodeStrict

encodeYamlFile
  :: HasCallStack
  => J.Encode a
  => FilePath
  -> a
  -> IO ()
encodeYamlFile file = Y.encodeFile file
  . either (error . show) id
  . Y.decodeEither' @Value
  . J.encodeStrict

encodeYamlFileWith
  :: HasCallStack
  => J.Encode a
  => Y.EncodeOptions
  -> FilePath
  -> a
  -> IO ()
encodeYamlFileWith opts file = Y.encodeFileWith opts file
  . either (error . show) id
  . Y.decodeEither' @Value
  . J.encodeStrict

