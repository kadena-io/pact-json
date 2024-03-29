{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module: Pact.JSON.Legacy.Hash
-- Copyright: Copyright © 2022 Kadena LLC.
-- License: BSD-3
--
-- Backward compatible hashing. Generally, the hash function preserves the
-- behavior of hashable-1.3.0.
--
-- For 'T.Text' values the behavior for text-1.5 is implemented. For other
-- provided instances for base types the behavior is independent of the version
-- of base that is used.
--
module Pact.JSON.Legacy.Hashable
( LegacyHashable(..)
, LegacyHashed(..)
, legacyHashSort
, hashable_fnv1
, hashable_fnv1_withSalt
, legacyDefaultSalt
) where

import Data.Aeson
import qualified Data.Aeson.Key as A
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as BS
import Data.Foldable
import Data.Hashable
import Data.Int
import qualified Data.List as L
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word

-- -------------------------------------------------------------------------- --
-- LegacyHashable Class

class Eq a => LegacyHashable a where
  legacyHash :: a -> Int
  legacyHashWithSalt :: Int -> a -> Int

-- | Legacy hash function for 'T.Text', that preserves the semantics of text-1.5
-- and hashable-1.3.0 on 64bit POSIX systems.
--
instance LegacyHashable T.Text where
  legacyHash = fromIntegral . hashable_fnv1 . T.encodeUtf16LE
  legacyHashWithSalt salt =
    fromIntegral . hashable_fnv1_withSalt (fromIntegral salt) . T.encodeUtf16LE
  {-# INLINE legacyHash #-}
  {-# INLINE legacyHashWithSalt #-}

instance LegacyHashable Key where
  legacyHash = legacyHash . A.toText
  legacyHashWithSalt s = legacyHashWithSalt s . A.toText
  {-# INLINE legacyHash #-}
  {-# INLINE legacyHashWithSalt #-}

instance LegacyHashable Int where
  legacyHash = id
  legacyHashWithSalt = defaultLegacyHashWithSalt
  {-# INLINE legacyHash #-}
  {-# INLINE legacyHashWithSalt #-}

instance LegacyHashable Word where
  legacyHash = fromIntegral
  legacyHashWithSalt = defaultLegacyHashWithSalt
  {-# INLINE legacyHash #-}
  {-# INLINE legacyHashWithSalt #-}

instance LegacyHashable () where
  legacyHash = fromEnum
  legacyHashWithSalt = defaultLegacyHashWithSalt
  {-# INLINE legacyHash #-}
  {-# INLINE legacyHashWithSalt #-}

instance LegacyHashable Bool where
  legacyHash = fromEnum
  legacyHashWithSalt = defaultLegacyHashWithSalt
  {-# INLINE legacyHash #-}
  {-# INLINE legacyHashWithSalt #-}

instance LegacyHashable Ordering where
  legacyHash = fromEnum
  legacyHashWithSalt = defaultLegacyHashWithSalt
  {-# INLINE legacyHash #-}
  {-# INLINE legacyHashWithSalt #-}

instance LegacyHashable Char where
  legacyHash = fromEnum
  legacyHashWithSalt = defaultLegacyHashWithSalt
  {-# INLINE legacyHash #-}
  {-# INLINE legacyHashWithSalt #-}

instance (LegacyHashable a1, LegacyHashable a2) => LegacyHashable (a1, a2) where
  legacyHash (a1, a2) = legacyHash a1 `legacyHashWithSalt` a2
  legacyHashWithSalt s (a1, a2) = s `legacyHashWithSalt` a1 `legacyHashWithSalt` a2
  {-# INLINE legacyHash #-}
  {-# INLINE legacyHashWithSalt #-}

instance LegacyHashable a => LegacyHashable [a] where
  legacyHash = legacyHashWithSalt legacyDefaultSalt
  legacyHashWithSalt salt = uncurry legacyHashWithSalt . foldl' go (salt, 0)
   where
    go :: LegacyHashable a => (Int, Int) -> a -> (Int, Int)
    go (!s, !l) x = (legacyHashWithSalt s x, l + 1)
  {-# SPECIALIZE instance LegacyHashable [Char] #-}

defaultLegacyHashWithSalt :: LegacyHashable a => Int -> a -> Int
defaultLegacyHashWithSalt salt = xor (salt * 16777619) . legacyHash
{-# INLINE defaultLegacyHashWithSalt #-}

legacyDefaultSalt :: Int
legacyDefaultSalt = -2578643520546668380

-- -------------------------------------------------------------------------- --
-- Use legacy hash with the Hashable class

newtype LegacyHashed a = LegacyHashed { _getLegacyHashed :: a }
  deriving newtype (Show, Eq, Ord, IsString, LegacyHashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

instance (LegacyHashable a) => Hashable (LegacyHashed a) where
  hash = legacyHash . _getLegacyHashed
  hashWithSalt s = legacyHashWithSalt s . _getLegacyHashed
  {-# INLINE hash #-}
  {-# INLINE hashWithSalt #-}

-- | Sort a list of 'T.Text' by using the legacy hash function.
--
legacyHashSort :: [T.Text] -> [T.Text]
legacyHashSort = L.sortOn legacyHash
{-# INLINE legacyHashSort #-}

-- -------------------------------------------------------------------------- --
-- Hashing of 'T.Text' for `text <2` and `hashable <1.3.1`

-- | Non-optimized implementation of FNV1 as used in hashable <1.3.1.
-- Only use for short inputs!
--
-- This implemetation deviates from the standard as follows
--
-- * It uses the 32 prime with 64bit arithmetic (on architectures where long has 8 bytes)
-- * It initializes the hash with a custom salt instead of FNV_offset_basis
--
hashable_fnv1_withSalt :: Int64 -> B.ByteString -> Int64
hashable_fnv1_withSalt = B.foldl' go
 where
  go :: Int64 -> Word8 -> Int64
  go h c = (h * fnv_prime_32) `xor` fromIntegral c

  -- 32 bit fnv1 prime
  fnv_prime_32 :: Int64
  fnv_prime_32 = 16777619

hashable_fnv1 :: B.ByteString -> Int64
hashable_fnv1 = hashable_fnv1_withSalt hashable_default_salt
 where
  hashable_default_salt :: Int64
  hashable_default_salt = -2578643520546668380
{-# INLINE hashable_fnv1 #-}

-- -------------------------------------------------------------------------- --
-- Hashing of ByteString and ShortByteString for hashable <1.3.1

-- The hash for ByteString and ShortByteString changes in version 1.3.1 (fixed
-- fnv1 implementation, see above) and 1.3.4.0 of the hashable package
-- (https://github.com/haskell-unordered-containers/hashable/pull/223). The
-- following code preserves the old behavior.
--
-- (The behavior for lazy bytestrings changes, too, but we don't include the
-- legacy behavior here.)

instance LegacyHashable BS.ShortByteString where
    legacyHash = legacyHash . BS.fromShort
    legacyHashWithSalt salt = legacyHashWithSalt salt . BS.fromShort
    {-# INLINE legacyHash #-}
    {-# INLINE legacyHashWithSalt #-}

instance LegacyHashable B.ByteString where
    legacyHash = fromIntegral . hashable_fnv1
    legacyHashWithSalt salt = fromIntegral . hashable_fnv1_withSalt (fromIntegral salt)
    {-# INLINE legacyHash #-}
    {-# INLINE legacyHashWithSalt #-}

