{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Pact.JSON.Legacy.Value
( -- LegacyValue
  LegacyValue(..)
, toLegacyJson

) where

import Control.DeepSeq

import Data.Aeson
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import Data.Bifunctor
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Typeable

import GHC.Stack

import Pact.JSON.Legacy.Hashable
import qualified Pact.JSON.Legacy.HashMap as LHM

import qualified Pact.JSON.Encode as J
import Pact.JSON.Legacy.Utils

-- -------------------------------------------------------------------------- --
-- Legacy JSON Value

-- | Legacy JSON Value
--
-- Use this type instead of 'Value' whenever a Value needs to be encoded. Do
-- not encode 'Value' values directly with 'encode' and similar functions.
--
-- This should rarely be needed. It is required only when values of "unknown" or
-- generic type are encoded as JSON, i.e. when 'Value' is (mis-)used as
-- generic/untyped data representation. In all other cases one should provide an
-- explicit 'Encode' instance and use 'LegacyHashMap' to achieve a backward
-- compatible ordering of properties.
--
-- Values of this types are stored as aeson 'Value' values in memory. Only the
-- encoding behavior is changed as follows:
--
-- * Calling 'toEncoding' orders object properties using the legacy order from
--   hashable-1.3.0 and unordered-containers-0.2.15.0
-- * Calling 'toJSON' is an 'error'.
--
newtype LegacyValue = LegacyValue { _getLegacyValue :: Value }
  deriving (Show, Eq, NFData)

instance ToJSON LegacyValue where
  toJSON :: LegacyValue -> Value
  toJSON =  _getLegacyValue
        -- this case is only safe when 'LegacyValues' are created. It allows to
        -- nest 'LegacyValue' values in inner datastructers into a new top-level
        -- 'LegacyValue'.
  {-# INLINE toJSON #-}

instance FromJSON LegacyValue where
  parseJSON = fmap LegacyValue . parseJSON
  {-# INLINE parseJSON #-}

-- | Convert a value with 'ToJSON' instance into 'LegacyValue'. The resulting
-- value must not be encoded with 'toJSON'. Instead the 'toEncoding' code path
-- must be used, which guarantees correct legacy ordering of properties.
--
-- In the context of toLegacyJson, we allow the use of toJSON on LegacyValue.
-- This allows to create aeson 'Value' values from types that have nested
-- 'LegacyValue' values. (This happens for instance in chainweb in pact service
-- when TxLog values are embedded in pact error messages.) It is safe to do so,
-- becaues by wrapping the overall result in 'LegacyValue' it is guaranteed that
-- the correct legacy serialization is preserved for all nested values. It is
-- also guaranteed that 'toJSON' can not be called in on the result to encode
-- the value to JSON.
--
toLegacyJson :: ToJSON a => a -> LegacyValue
toLegacyJson = LegacyValue . toJSON
{-# INLINE toLegacyJson #-}

-- | Encode an aeson 'Value' in a way that is compatible with aeson <2
--
instance J.Encode LegacyValue where
  build (LegacyValue (Object o)) = J.build $ LegacyValue <$> legacyKeyMap o
  build (LegacyValue (Array a)) = J.build $ J.Array $ LegacyValue <$> a
  build (LegacyValue v) = J.build v

  {-# INLINE build #-}

