{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Numeric.Decimal
  (
  -- * Arithmetic
    module Numeric.Decimal.BoundedArithmetic
  , module Numeric.Decimal.Internal
  -- * Rounding
  -- ** Round half up
  , RoundHalfUp
  , roundHalfUp
  -- ** Round half down
  , RoundHalfDown
  , roundHalfDown
  -- ** Round half even
  , RoundHalfEven
  , roundHalfEven
  -- ** Round half to zero
  , RoundHalfToZero
  , roundHalfToZero
  -- ** Round half from zero
  , RoundHalfFromZero
  , roundHalfFromZero
  -- ** Round down
  , RoundDown
  , Floor
  , roundDown
  -- ** Round up
  , RoundUp
  , Ceiling
  , roundUp
  -- ** Round towards zero
  , RoundToZero
  , Truncate
  , roundToZero
  -- ** Round away from zero
  , RoundFromZero
  , roundFromZero
  -- * Operations
  , decimalList
  , sumDecimalBounded
  , productDecimalBoundedWithRounding
  -- * Conversion
  -- ** Fixed
  , FixedScale
  , toFixedDecimal
  , fromFixedDecimal
  , fromFixedDecimalBounded
  -- ** Scientific
  , toScientificDecimal
  , fromScientificDecimal
  , fromScientificDecimalBounded
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.Catch
import Data.Coerce
import Data.Fixed
import Data.Int
import Data.Proxy
import Data.Scientific
import Data.Word
import GHC.TypeLits
import Numeric.Decimal.BoundedArithmetic
import Numeric.Decimal.Internal


-- | [Round half up](https://en.wikipedia.org/wiki/Rounding#Round_half_up) rounding strategy:
--
-- >>> :set -XDataKinds
-- >>> roundDecimal <$> (3.740 :: Arith (Decimal RoundHalfUp 3 Int)) :: Arith (Decimal RoundHalfUp 1 Int)
-- Arith 3.7
--
-- Or with a bit more concise approach using `arithRoundD` and @TypeApplications@:
--
-- >>> :set -XTypeApplications
-- >>> arithRoundD @1 @RoundHalfUp @3 @Int 3.740
-- Arith 3.7
-- >>> arithRoundD @1 @RoundHalfUp @3 @Int 3.749
-- Arith 3.7
-- >>> arithRoundD @1 @RoundHalfUp @3 @Int 3.750
-- Arith 3.8
-- >>> arithRoundD @1 @RoundHalfUp @3 @Int 3.751
-- Arith 3.8
-- >>> arithRoundD @1 @RoundHalfUp @3 @Int 3.760
-- Arith 3.8
-- >>> arithRoundD @1 @RoundHalfUp @3 @Int (-3.740)
-- Arith -3.7
-- >>> arithRoundD @1 @RoundHalfUp @3 @Int (-3.749)
-- Arith -3.7
-- >>> arithRoundD @1 @RoundHalfUp @3 @Int (-3.750)
-- Arith -3.7
-- >>> arithRoundD @1 @RoundHalfUp @3 @Int (-3.751)
-- Arith -3.8
-- >>> arithRoundD @1 @RoundHalfUp @3 @Int (-3.760)
-- Arith -3.8
--
-- @since 0.1.0
data RoundHalfUp

instance Round RoundHalfUp Integer where
  roundDecimal = roundHalfUp
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfUp Int where
  roundDecimal = roundHalfUp
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfUp Int8 where
  roundDecimal = roundHalfUp
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfUp Int16 where
  roundDecimal = roundHalfUp
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfUp Int32 where
  roundDecimal = roundHalfUp
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfUp Int64 where
  roundDecimal = roundHalfUp
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfUp Word where
  roundDecimal = roundHalfUp
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfUp Word8 where
  roundDecimal = roundHalfUp
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfUp Word16 where
  roundDecimal = roundHalfUp
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfUp Word32 where
  roundDecimal = roundHalfUp
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfUp Word64 where
  roundDecimal = roundHalfUp
  {-# INLINABLE roundDecimal #-}

roundHalfUp :: forall r n k p . (Integral p, KnownNat k) => Decimal r (n + k) p -> Decimal r n p
roundHalfUp (Decimal x)
    | k == 0                     = Decimal x
    | r >= s1                    = Decimal (q + 1)
    | signum r < 0 && abs r > s1 = Decimal (q - 1)
    | otherwise                  = Decimal q
    where
      k = fromIntegral (natVal (Proxy :: Proxy k)) :: Int
      s1 = 10 ^ k
      (q, r) = (2 *) <$> quotRem x s1
{-# INLINABLE roundHalfUp #-}

-- | [Round half down](https://en.wikipedia.org/wiki/Rounding#Round_half_down) rounding strategy:
--
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> arithRoundD @1 @RoundHalfDown @3 @Int 3.740
-- Arith 3.7
-- >>> arithRoundD @1 @RoundHalfDown @3 @Int 3.749
-- Arith 3.7
-- >>> arithRoundD @1 @RoundHalfDown @3 @Int 3.750
-- Arith 3.7
-- >>> arithRoundD @1 @RoundHalfDown @3 @Int 3.751
-- Arith 3.8
-- >>> arithRoundD @1 @RoundHalfDown @3 @Int 3.760
-- Arith 3.8
-- >>> arithRoundD @1 @RoundHalfDown @3 @Int (-3.740)
-- Arith -3.7
-- >>> arithRoundD @1 @RoundHalfDown @3 @Int (-3.749)
-- Arith -3.7
-- >>> arithRoundD @1 @RoundHalfDown @3 @Int (-3.750)
-- Arith -3.8
-- >>> arithRoundD @1 @RoundHalfDown @3 @Int (-3.751)
-- Arith -3.8
-- >>> arithRoundD @1 @RoundHalfDown @3 @Int (-3.760)
-- Arith -3.8
--
-- @since 0.2.0
data RoundHalfDown

instance Round RoundHalfDown Integer where
  roundDecimal = roundHalfDown
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfDown Int where
  roundDecimal = roundHalfDown
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfDown Int8 where
  roundDecimal = roundHalfDown
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfDown Int16 where
  roundDecimal = roundHalfDown
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfDown Int32 where
  roundDecimal = roundHalfDown
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfDown Int64 where
  roundDecimal = roundHalfDown
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfDown Word where
  roundDecimal = roundHalfDown
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfDown Word8 where
  roundDecimal = roundHalfDown
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfDown Word16 where
  roundDecimal = roundHalfDown
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfDown Word32 where
  roundDecimal = roundHalfDown
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfDown Word64 where
  roundDecimal = roundHalfDown
  {-# INLINABLE roundDecimal #-}

roundHalfDown :: forall r n k p . (Integral p, KnownNat k) => Decimal r (n + k) p -> Decimal r n p
roundHalfDown (Decimal x)
    | k == 0                      = Decimal x
    | r > s1                      = Decimal (q + 1)
    | signum r < 0 && abs r >= s1 = Decimal (q - 1)
    | otherwise                   = Decimal q
    where
      k = fromIntegral (natVal (Proxy :: Proxy k)) :: Int
      s1 = 10 ^ k
      (q, r) = (2 *) <$> quotRem x s1
{-# INLINABLE roundHalfDown #-}

-- | [Round half even](https://en.wikipedia.org/wiki/Rounding#Round_half_to_even) rounding
-- strategy. If the fractional part of x is 0.5, then y is the even integer nearest to
-- x. This is the default rounding strategy in Haskell implemented by `round`.
--
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> arithRoundD @1 @RoundHalfEven @3 @Int 3.650
-- Arith 3.6
-- >>> arithRoundD @1 @RoundHalfEven @3 @Int 3.740
-- Arith 3.7
-- >>> arithRoundD @1 @RoundHalfEven @3 @Int 3.749
-- Arith 3.7
-- >>> arithRoundD @1 @RoundHalfEven @3 @Int 3.750
-- Arith 3.8
-- >>> arithRoundD @1 @RoundHalfEven @3 @Int 3.751
-- Arith 3.8
-- >>> arithRoundD @1 @RoundHalfEven @3 @Int 3.760
-- Arith 3.8
-- >>> arithRoundD @1 @RoundHalfEven @3 @Int (-3.650)
-- Arith -3.6
-- >>> arithRoundD @1 @RoundHalfEven @3 @Int (-3.740)
-- Arith -3.7
-- >>> arithRoundD @1 @RoundHalfEven @3 @Int (-3.749)
-- Arith -3.7
-- >>> arithRoundD @1 @RoundHalfEven @3 @Int (-3.750)
-- Arith -3.8
-- >>> arithRoundD @1 @RoundHalfEven @3 @Int (-3.751)
-- Arith -3.8
-- >>> arithRoundD @1 @RoundHalfEven @3 @Int (-3.760)
-- Arith -3.8
--
-- @since 0.2.0
data RoundHalfEven

instance Round RoundHalfEven Integer where
  roundDecimal = roundHalfEven
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfEven Int where
  roundDecimal = roundHalfEven
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfEven Int8 where
  roundDecimal = roundHalfEven
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfEven Int16 where
  roundDecimal = roundHalfEven
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfEven Int32 where
  roundDecimal = roundHalfEven
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfEven Int64 where
  roundDecimal = roundHalfEven
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfEven Word where
  roundDecimal = roundHalfEven
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfEven Word8 where
  roundDecimal = roundHalfEven
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfEven Word16 where
  roundDecimal = roundHalfEven
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfEven Word32 where
  roundDecimal = roundHalfEven
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfEven Word64 where
  roundDecimal = roundHalfEven
  {-# INLINABLE roundDecimal #-}

roundHalfEven :: forall r n k p . (Integral p, KnownNat k) => Decimal r (n + k) p -> Decimal r n p
roundHalfEven (Decimal x)
    | k == 0                     = Decimal x
    | abs r == s1 && odd q       = Decimal (q + signum r)
    | abs r == s1                = Decimal q
    | r > s1                     = Decimal (q + 1)
    | signum r < 0 && abs r > s1 = Decimal (q - 1)
    | otherwise                  = Decimal q
    where
      k = fromIntegral (natVal (Proxy :: Proxy k)) :: Int
      s1 = 10 ^ k
      (q, r) = (2 *) <$> quotRem x s1
{-# INLINABLE roundHalfEven #-}

-- | [Round half towards zero](https://en.wikipedia.org/wiki/Rounding#Round_half_towards_zero) rounding
-- strategy. If the fraction of x is exactly 0.5, then y = x − 0.5 if x is positive, and y = x + 0.5 if x is negative.
--
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> arithRoundD @1 @RoundHalfToZero @3 @Int 3.650
-- Arith 3.6
-- >>> arithRoundD @1 @RoundHalfToZero @3 @Int 3.740
-- Arith 3.7
-- >>> arithRoundD @1 @RoundHalfToZero @4 @Int 3.7501
-- Arith 3.8
-- >>> arithRoundD @1 @RoundHalfToZero @3 @Int (-3.650)
-- Arith -3.6
-- >>> arithRoundD @1 @RoundHalfToZero @3 @Int (-3.740)
-- Arith -3.7
-- >>> arithRoundD @1 @RoundHalfToZero @4 @Int (-3.7501)
-- Arith -3.8
-- >>> arithRoundD @1 @RoundHalfToZero @3 @Int (-3.760)
-- Arith -3.8

-- @since 0.2.0
data RoundHalfToZero

instance Round RoundHalfToZero Integer where
  roundDecimal = roundHalfToZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfToZero Int where
  roundDecimal = roundHalfToZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfToZero Int8 where
  roundDecimal = roundHalfToZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfToZero Int16 where
  roundDecimal = roundHalfToZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfToZero Int32 where
  roundDecimal = roundHalfToZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfToZero Int64 where
  roundDecimal = roundHalfToZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfToZero Word where
  roundDecimal = roundHalfToZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfToZero Word8 where
  roundDecimal = roundHalfToZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfToZero Word16 where
  roundDecimal = roundHalfToZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfToZero Word32 where
  roundDecimal = roundHalfToZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfToZero Word64 where
  roundDecimal = roundHalfToZero
  {-# INLINABLE roundDecimal #-}

roundHalfToZero :: forall r n k p . (Integral p, KnownNat k) => Decimal r (n + k) p -> Decimal r n p
roundHalfToZero (Decimal x)
    | k == 0                     = Decimal x
    | r > s1                     = Decimal (q + 1)
    | signum r < 0 && abs r > s1 = Decimal (q - 1)
    | otherwise                  = Decimal q
    where
      k = fromIntegral (natVal (Proxy :: Proxy k)) :: Int
      s1 = 10 ^ k
      (q, r) = (2 *) <$> quotRem x s1
{-# INLINABLE roundHalfToZero #-}

-- | [Round half away from zero](https://en.wikipedia.org/wiki/Rounding#Round_half_away_from_zero) rounding
-- strategy. If the fraction of x is exactly 0.5, then y = x + 0.5 if x is positive, and y = x − 0.5 if x is negative.
--
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> arithRoundD @1 @RoundHalfFromZero @3 @Int 3.650
-- Arith 3.7
-- >>> arithRoundD @1 @RoundHalfFromZero @3 @Int 3.740
-- Arith 3.7
-- >>> arithRoundD @1 @RoundHalfFromZero @3 @Int 3.751
-- Arith 3.8
-- >>> arithRoundD @1 @RoundHalfFromZero @3 @Int (-3.650)
-- Arith -3.7
-- >>> arithRoundD @1 @RoundHalfFromZero @3 @Int (-3.740)
-- Arith -3.7
-- >>> arithRoundD @1 @RoundHalfFromZero @3 @Int (-3.751)
-- Arith -3.8
-- >>> arithRoundD @1 @RoundHalfFromZero @3 @Int (-3.760)
-- Arith -3.8

-- @since 0.2.0
data RoundHalfFromZero

instance Round RoundHalfFromZero Integer where
  roundDecimal = roundHalfFromZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfFromZero Int where
  roundDecimal = roundHalfFromZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfFromZero Int8 where
  roundDecimal = roundHalfFromZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfFromZero Int16 where
  roundDecimal = roundHalfFromZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfFromZero Int32 where
  roundDecimal = roundHalfFromZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfFromZero Int64 where
  roundDecimal = roundHalfFromZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfFromZero Word where
  roundDecimal = roundHalfFromZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfFromZero Word8 where
  roundDecimal = roundHalfFromZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfFromZero Word16 where
  roundDecimal = roundHalfFromZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfFromZero Word32 where
  roundDecimal = roundHalfFromZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundHalfFromZero Word64 where
  roundDecimal = roundHalfFromZero
  {-# INLINABLE roundDecimal #-}

roundHalfFromZero :: forall r n k p . (Integral p, KnownNat k) => Decimal r (n + k) p -> Decimal r n p
roundHalfFromZero (Decimal x)
    | k == 0                      = Decimal x
    | r >= s1                     = Decimal (q + 1)
    | signum r < 0 && abs r >= s1 = Decimal (q - 1)
    | otherwise                   = Decimal q
    where
      k = fromIntegral (natVal (Proxy :: Proxy k)) :: Int
      s1 = 10 ^ k
      (q, r) = (2 *) <$> quotRem x s1
{-# INLINABLE roundHalfFromZero #-}

-- | [Round down](https://en.wikipedia.org/wiki/Rounding#Rounding_down) rounding
-- startegy. This the strategy that is implemented by `floor`. Round towards minus
-- infinity:
--
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> arithRoundD @1 @RoundDown @2 @Int 3.65
-- Arith 3.6
-- >>> arithRoundD @1 @RoundDown @2 @Int 3.75
-- Arith 3.7
-- >>> arithRoundD @1 @RoundDown @2 @Int 3.89
-- Arith 3.8
-- >>> arithRoundD @1 @RoundDown @2 @Int (-3.65)
-- Arith -3.7
--
-- @since 0.2.0
data RoundDown

-- | Synonym for round down
--
-- @since 0.2.0
type Floor = RoundDown

instance Round RoundDown Integer where
  roundDecimal = roundDown
  {-# INLINABLE roundDecimal #-}
instance Round RoundDown Int where
  roundDecimal = roundDown
  {-# INLINABLE roundDecimal #-}
instance Round RoundDown Int8 where
  roundDecimal = roundDown
  {-# INLINABLE roundDecimal #-}
instance Round RoundDown Int16 where
  roundDecimal = roundDown
  {-# INLINABLE roundDecimal #-}
instance Round RoundDown Int32 where
  roundDecimal = roundDown
  {-# INLINABLE roundDecimal #-}
instance Round RoundDown Int64 where
  roundDecimal = roundDown
  {-# INLINABLE roundDecimal #-}
instance Round RoundDown Word where
  roundDecimal = roundDown
  {-# INLINABLE roundDecimal #-}
instance Round RoundDown Word8 where
  roundDecimal = roundDown
  {-# INLINABLE roundDecimal #-}
instance Round RoundDown Word16 where
  roundDecimal = roundDown
  {-# INLINABLE roundDecimal #-}
instance Round RoundDown Word32 where
  roundDecimal = roundDown
  {-# INLINABLE roundDecimal #-}
instance Round RoundDown Word64 where
  roundDecimal = roundDown
  {-# INLINABLE roundDecimal #-}

roundDown :: forall r n k p . (Integral p, KnownNat k) => Decimal r (n + k) p -> Decimal r n p
roundDown (Decimal x)
  | x >= 0 || r == 0 = Decimal q
  | otherwise = Decimal (q - 1)
  where
    k = fromIntegral (natVal (Proxy :: Proxy k)) :: Int
    (q, r) = quotRem x (10 ^ k)
{-# INLINABLE roundDown #-}



-- | [Round up](https://en.wikipedia.org/wiki/Rounding#Rounding_up) rounding
-- startegy. This the strategy that is implemented by `celing`. Round towards positive
-- infinity:
--
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> arithRoundD @1 @RoundUp @2 @Int 3.65
-- Arith 3.7
-- >>> arithRoundD @1 @RoundUp @2 @Int 3.75
-- Arith 3.8
-- >>> arithRoundD @1 @RoundUp @2 @Int 3.89
-- Arith 3.9
-- >>> arithRoundD @1 @RoundUp @2 @Int (-3.65)
-- Arith -3.6
--
-- @since 0.2.2
data RoundUp

-- | Synonym for round up
--
-- @since 0.2.2
type Ceiling = RoundUp

instance Round RoundUp Integer where
  roundDecimal = roundUp
  {-# INLINABLE roundDecimal #-}
instance Round RoundUp Int where
  roundDecimal = roundUp
  {-# INLINABLE roundDecimal #-}
instance Round RoundUp Int8 where
  roundDecimal = roundUp
  {-# INLINABLE roundDecimal #-}
instance Round RoundUp Int16 where
  roundDecimal = roundUp
  {-# INLINABLE roundDecimal #-}
instance Round RoundUp Int32 where
  roundDecimal = roundUp
  {-# INLINABLE roundDecimal #-}
instance Round RoundUp Int64 where
  roundDecimal = roundUp
  {-# INLINABLE roundDecimal #-}
instance Round RoundUp Word where
  roundDecimal = roundUp
  {-# INLINABLE roundDecimal #-}
instance Round RoundUp Word8 where
  roundDecimal = roundUp
  {-# INLINABLE roundDecimal #-}
instance Round RoundUp Word16 where
  roundDecimal = roundUp
  {-# INLINABLE roundDecimal #-}
instance Round RoundUp Word32 where
  roundDecimal = roundUp
  {-# INLINABLE roundDecimal #-}
instance Round RoundUp Word64 where
  roundDecimal = roundUp
  {-# INLINABLE roundDecimal #-}

roundUp :: forall r n k p . (Integral p, KnownNat k) => Decimal r (n + k) p -> Decimal r n p
roundUp (Decimal x)
  | x >= 0 && r /= 0 = Decimal (q + 1)
  | otherwise = Decimal q
  where
    k = fromIntegral (natVal (Proxy :: Proxy k)) :: Int
    (q, r) = quotRem x (10 ^ k)
{-# INLINABLE roundUp #-}

-- | [Round towards zero](https://en.wikipedia.org/wiki/Rounding#Round_towards_zero)
-- strategy. Similar to Haskell's `truncate`. Drop the fractional digits, regardless of
-- the sign.
--
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> arithRoundD @1 @RoundToZero @2 @Int 3.65
-- Arith 3.6
-- >>> arithRoundD @1 @RoundToZero @2 @Int 3.75
-- Arith 3.7
-- >>> arithRoundD @1 @RoundToZero @2 @Int 3.89
-- Arith 3.8
-- >>> arithRoundD @1 @RoundToZero @2 @Int (-3.65)
-- Arith -3.6
--
-- @since 0.2.0
data RoundToZero


-- | Synonym for `RoundToZero`
--
-- @since 0.1.0
type Truncate = RoundToZero

instance Round RoundToZero Integer where
  roundDecimal = roundToZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundToZero Int where
  roundDecimal = roundToZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundToZero Int8 where
  roundDecimal = roundToZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundToZero Int16 where
  roundDecimal = roundToZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundToZero Int32 where
  roundDecimal = roundToZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundToZero Int64 where
  roundDecimal = roundToZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundToZero Word where
  roundDecimal = roundToZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundToZero Word8 where
  roundDecimal = roundToZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundToZero Word16 where
  roundDecimal = roundToZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundToZero Word32 where
  roundDecimal = roundToZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundToZero Word64 where
  roundDecimal = roundToZero
  {-# INLINABLE roundDecimal #-}

roundToZero :: forall r n k p . (Integral p, KnownNat k) => Decimal r (n + k) p -> Decimal r n p
roundToZero (Decimal x) = Decimal (quot x (10 ^ k))
  where
    k = fromIntegral (natVal (Proxy :: Proxy k)) :: Int
{-# INLINABLE roundToZero #-}


-- | [Round away from zero](https://en.wikipedia.org/wiki/Rounding#Rounding_away_from_zero)
-- strategy. Also known as round towards infinity.
--
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> arithRoundD @1 @RoundFromZero @2 @Int 3.65
-- Arith 3.7
-- >>> arithRoundD @1 @RoundFromZero @2 @Int 3.75
-- Arith 3.8
-- >>> arithRoundD @1 @RoundFromZero @2 @Int 3.89
-- Arith 3.9
-- >>> arithRoundD @1 @RoundFromZero @2 @Int (-3.65)
-- Arith -3.7
--
-- @since 0.2.2
data RoundFromZero


instance Round RoundFromZero Integer where
  roundDecimal = roundFromZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundFromZero Int where
  roundDecimal = roundFromZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundFromZero Int8 where
  roundDecimal = roundFromZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundFromZero Int16 where
  roundDecimal = roundFromZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundFromZero Int32 where
  roundDecimal = roundFromZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundFromZero Int64 where
  roundDecimal = roundFromZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundFromZero Word where
  roundDecimal = roundFromZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundFromZero Word8 where
  roundDecimal = roundFromZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundFromZero Word16 where
  roundDecimal = roundFromZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundFromZero Word32 where
  roundDecimal = roundFromZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundFromZero Word64 where
  roundDecimal = roundFromZero
  {-# INLINABLE roundDecimal #-}

roundFromZero :: forall r n k p . (Integral p, KnownNat k) => Decimal r (n + k) p -> Decimal r n p
roundFromZero (Decimal x) = Decimal (q + signum r)
  where
    k = fromIntegral (natVal (Proxy :: Proxy k)) :: Int
    (q, r) = quotRem x (10 ^ k)
{-# INLINABLE roundFromZero #-}

-- | /O(1)/ - Conversion of a list.
--
-- __Note__: It doesn't do any scaling, eg:
--
-- >>> :set -XDataKinds
-- >>> import Numeric.Decimal
-- >>> decimalList [1,20,300] :: [Decimal RoundHalfUp 2 Int]
-- [0.01,0.20,3.00]
--
-- If scaling is what you need use `fromIntegral` instead:
--
-- >>> sequenceA [1, 20, 300] :: Arith [Decimal RoundHalfUp 2 Int]
-- Arith [1.00,20.00,300.00]
--
-- @since 0.1.0
decimalList :: Integral p => [p] -> [Decimal r s p]
decimalList = coerce


-- | Sum a list of decimal numbers
--
-- >>> :set -XDataKinds
-- >>> sequenceA [1.1, 20.02, 300.003] >>= sumDecimalBounded :: Arith (Decimal RoundHalfUp 3 Int)
-- Arith 321.123
--
-- @since 0.2.0
sumDecimalBounded ::
     (MonadThrow m, Foldable f, Eq p, Ord p, Num p, Bounded p)
  => f (Decimal r s p)
  -> m (Decimal r s p)
sumDecimalBounded = foldM plusDecimalBounded (Decimal 0)
{-# INLINABLE sumDecimalBounded #-}

-- | Multiply all decimal numbers in the list while doing rounding.
--
-- >>> :set -XDataKinds
-- >>> product [1.1, 20.02, 300.003] :: Double
-- 6606.666066000001
-- >>> xs <- arithM (mapM fromRational [1.1, 20.02, 300.003] :: Arith [Decimal RoundHalfUp 4 Int])
-- >>> xs
-- [1.1000,20.0200,300.0030]
-- >>> productDecimalBoundedWithRounding xs
-- 6606.6661
--
-- @since 0.2.0
productDecimalBoundedWithRounding ::
     (MonadThrow m, Foldable f, KnownNat s, Round r Integer, Integral p, Bounded p)
  => f (Decimal r s p)
  -> m (Decimal r s p)
productDecimalBoundedWithRounding ds =
  fromIntegralDecimalBounded 1 >>=
  (\acc -> foldM timesDecimalBoundedWithRounding acc ds)
{-# INLINABLE productDecimalBoundedWithRounding #-}


---- Scientific


-- | Convert a `Decimal` to `Scientific`
--
-- @since 0.1.0
toScientificDecimal :: (Integral p, KnownNat s) => Decimal r s p -> Scientific
toScientificDecimal dec =
  scientific
    (toInteger (unwrapDecimal dec))
    (fromInteger (negate (getScale dec)))

-- | Convert Scientific to Decimal without loss of precision. Will return `Left` `Underflow` if
-- `Scientific` has too many decimal places, more than `Decimal` scaling is capable to handle.
--
-- @since 0.1.0
fromScientificDecimal ::
     forall m r s. (MonadThrow m, KnownNat s)
  => Scientific
  -> m (Decimal r s Integer)
fromScientificDecimal numNonNormal
  | exp10 > s = throwM Underflow
  | otherwise = pure (Decimal (coefficient num * 10 ^ (s - exp10)))
  where
    num = normalize numNonNormal
    s = natVal (Proxy :: Proxy s)
    exp10 = negate (toInteger (base10Exponent num))

-- | Convert from Scientific to bounded Decimal while checking for Overflow/Underflow
--
-- @since 0.1.0
fromScientificDecimalBounded ::
     forall m r s p. (MonadThrow m, Integral p, Bounded p, KnownNat s)
  => Scientific
  -> m (Decimal r s p)
fromScientificDecimalBounded numNonNormal = do
  when (coeff < toInteger (minBound :: p) || exp10 > s) $ throwM Underflow
  when (coeff > imax || posExp10 > upperExponentBound || scaledCoeff > imax) $ throwM Overflow
  pure (Decimal (fromInteger scaledCoeff))
  where
    num = normalize numNonNormal
    s = natVal (Proxy :: Proxy s)
    posExp10 = toInteger (base10Exponent num)
    exp10 = negate posExp10
    imax = toInteger (maxBound :: p)
    coeff = coefficient num
    scaledCoeff = coefficient num * 10 ^ (s - exp10)
    upperExponentBound = ceiling (logBase 10 $ fromIntegral (maxBound :: p) :: Double) - s


type family FixedScale e :: Nat

type instance FixedScale E0 = 0
type instance FixedScale E1 = 1
type instance FixedScale E2 = 2
type instance FixedScale E3 = 3
type instance FixedScale E6 = 6
type instance FixedScale E9 = 9
type instance FixedScale E12 = 12


-- | Convert a `Decimal` to a `Fixed` with the exactly same precision.
--
-- >>> toFixedDecimal <$> (3.65 :: Arith (Decimal RoundDown 2 Int)) :: Arith (Fixed E2)
-- Arith 3.65
-- >>> toFixedDecimal $ fromFixedDecimal (123.45 :: Fixed E2) :: Fixed E2
-- 123.45
--
-- @since 0.2.0
toFixedDecimal :: (s ~ FixedScale e, Integral p) => Decimal r s p -> Fixed e
toFixedDecimal = MkFixed . toInteger . unwrapDecimal

-- | Convert a `Fixed` to a `Decimal` with the exactly same precision
--
-- >>> fromFixedDecimal (123.45 :: Fixed E2)
-- 123.45
--
-- @since 0.2.0
fromFixedDecimal :: s ~ FixedScale e => Fixed e -> Decimal r s Integer
fromFixedDecimal = coerce

-- | Convert a `Fixed` to a decimal backed by a bounded integral with the exactly same
-- precision
--
-- >>> fromFixedDecimalBounded (123.458 :: Fixed E3) :: Arith (Decimal RoundToZero 3 Int)
-- Arith 123.458
-- >>> fromFixedDecimalBounded (123.458 :: Fixed E3) :: Arith (Decimal RoundToZero 3 Int8)
-- ArithError arithmetic overflow
-- >>> fromFixedDecimalBounded (-123.458 :: Fixed E3) :: Arith (Decimal RoundToZero 3 Word)
-- ArithError arithmetic underflow
--
-- @since 0.2.0
fromFixedDecimalBounded ::
     (s ~ FixedScale e, MonadThrow m, Integral p, Bounded p)
  => Fixed e
  -> m (Decimal r s p)
fromFixedDecimalBounded = fromIntegerDecimalBounded . fromFixedDecimal
