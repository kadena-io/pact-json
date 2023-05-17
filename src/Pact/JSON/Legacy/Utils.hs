{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Pact.JSON.Legacy.Utils
-- Copyright: Copyright © 2023 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Pact.JSON.Legacy.Utils
( legacyKeyMap
, legacyMap
, legacyMap_
, legacyHashMap
, legacyHashMap_

-- * Tools
, legacyJsonPropertySort
, legacyJsonPropertySortPairs
, legacyHashSetToList
) where

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

-- -------------------------------------------------------------------------- --
-- Utils

-- | Transform a list of key value pairs with a textual JSON key encoding into a
-- list of pairs with text keys.
--
-- It is an error to call this function on pairs with non-textual keys.
--
-- Often, instead of forcing Text Keys we would also just require
-- 'LegacyHashable' for the key type, which would avoid the 'error'. However, we
-- think (and have evidence), that a non-textual encoding is almost always
-- uninteded and enforcing a textual encoding catches those issues.
--
enforceTextKeys
  :: forall k v
  . HasCallStack
  => Typeable k
  => ToJSONKey k
  => [(k, v)]
  -> [(T.Text, v)]
enforceTextKeys = case toJSONKey of
  ToJSONKeyText f _ -> fmap (first (AK.toText . f))
  _ -> error $ "Pact.Utils.Json: failed to JSON encode non-textual legacy key of type " <> show (typeRep (Proxy @k))
{-# INLINE enforceTextKeys #-}

-- -------------------------------------------------------------------------- --
-- Aeson KeyMap Conversion

-- | Convert a 'AKM.KeyMap' that has a textual JSON key into LegacyHashMap which
-- perserve the legacy ordering of keys.
--
legacyKeyMap :: HasCallStack => AKM.KeyMap v -> LHM.HashMap T.Text v
legacyKeyMap = LHM.fromList . fmap (first AK.toText) . AKM.toList
{-# INLINE legacyKeyMap #-}

-- -------------------------------------------------------------------------- --
-- Strict Map Conversion

-- | Convert a 'M.Map' that has a textual JSON key into HashMap with a Legacy
-- Key.
--
-- It is an error if the key type is not encoded as JSON text.
--
-- Instead of forcing text Keys we would also just require 'LegacyHashable' for
-- the key type, which would avoid the 'error'. However, we think (and have
-- evidence), that a non-textual encoding is almost always uninteded and
-- enforcing a textual encoding catches those issues.
--
legacyMap
  :: HasCallStack
  => Typeable a
  => ToJSONKey a
  => M.Map a b
  -> LHM.HashMap T.Text b
legacyMap = LHM.fromList . enforceTextKeys . M.toList
{-# INLINE legacyMap #-}

-- | A version of legacyMap that does not enforce textual keys but instead
-- requires a 'LegacyHashable' constraint.
--
legacyMap_ :: LegacyHashable a => M.Map a b -> LHM.HashMap a b
legacyMap_ = LHM.fromList . M.toList
{-# INLINE legacyMap_ #-}

-- -------------------------------------------------------------------------- --
-- Strict HashMap Conversion

-- | Convert a 'HM.HashMap.Strict' that has a textual JSON key into HashMap with
-- a Legacy Key.
--
-- It is an error if the key type is not encoded as JSON text.
--
-- Instead of forcing text Keys we would also just require 'LegacyHashable' for
-- the key type, which would avoid the 'error'. However, we think (and have
-- evidence), that a non-textual encoding is almost always uninteded and
-- enforcing a textual encoding catches those issues.
--
-- That said, there are occurences of Hashmaps with non-textual keys (even for
-- key types that have ToJSONKey instances) where the map is encoded as list of
-- pairs (lists of length 2), which is the default encoding in aeson. For these
-- (probaby unintentional) special cases we also provide 'legacyHashMap_' which
-- has  'LegacyHashable' constraint for the key type.
--
legacyHashMap
  :: HasCallStack
  => Typeable a
  => ToJSONKey a
  => HM.HashMap a b
  -> LHM.HashMap T.Text b
legacyHashMap = LHM.fromList . enforceTextKeys . HM.toList
{-# INLINE legacyHashMap #-}

-- | A version of legacyMap that does not enforce textual keys but instead
-- requires a 'LegacyHashable' constraint.
--
legacyHashMap_ :: LegacyHashable a => HM.HashMap a b -> LHM.HashMap a b
legacyHashMap_ = LHM.fromList . HM.toList
{-# INLINE legacyHashMap_ #-}

-- -------------------------------------------------------------------------- --
-- Tools

-- | Sort a list of 'T.Text' using the legacy JSON property sorting.
--
legacyJsonPropertySort :: [T.Text] -> [T.Text]
legacyJsonPropertySort = LHM.sort
{-# INLINE legacyJsonPropertySort #-}

-- | Sort a list of 'T.Text' using the legacy JSON property sorting.
--
legacyJsonPropertySortPairs :: [T.Text] -> [T.Text]
legacyJsonPropertySortPairs = LHM.sort
{-# INLINE legacyJsonPropertySortPairs #-}

legacyHashSetToList :: HS.HashSet T.Text -> [T.Text]
legacyHashSetToList = legacyJsonPropertySort . HS.toList
{-# INLINE legacyHashSetToList #-}

