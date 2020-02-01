{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Numeric.Decimal
  ( Decimal64
  -- * Rounding
  -- ** Round half up
  , RoundHalfUp
  -- ** Round half down
  , RoundHalfDown
  -- ** Round half even
  , RoundHalfEven
  -- ** Round down
  , RoundDown
  , Floor
  -- ** Round towards zero
  , RoundToZero
  , Truncate
  -- * Arithmetic
  , module Numeric.Decimal.BoundedArithmetic
  , module Numeric.Decimal.Internal
  -- * Operations
  , decimalList
  , sumDecimalBounded
  , productDecimalBoundedWithRounding
  -- * Conversion
  -- ** Fixed
  , FixedScale
  , toFixed
  , fromFixed
  , fromFixedBounded
  -- ** Scientific
  , toScientific
  , fromScientific
  , fromScientificBounded
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.Catch
import Data.Coerce
import Data.Fixed
import Data.Int
import Data.Word
import Data.Proxy
import Data.Scientific
import GHC.TypeLits
import Numeric.Decimal.BoundedArithmetic
import Numeric.Decimal.Internal

-- | A pretty common Decimal type backed by `Int64` and standard rounding
type Decimal64 s = Decimal RoundHalfUp s Int64


-- | [Round half up](https://en.wikipedia.org/wiki/Rounding#Round_half_up): A very common
-- rounding strategy:
--
-- >>> :set -XDataKinds
-- >>> roundDecimal <$> (3.740 :: IO (Decimal RoundHalfUp 3 Int)) :: IO (Decimal RoundHalfUp 1 Int)
-- 3.7
-- >>> roundDecimal <$> (3.749 :: IO (Decimal RoundHalfUp 3 Int)) :: IO (Decimal RoundHalfUp 1 Int)
-- 3.7
-- >>> roundDecimal <$> (3.750 :: IO (Decimal RoundHalfUp 3 Int)) :: IO (Decimal RoundHalfUp 1 Int)
-- 3.8
-- >>> roundDecimal <$> (3.751 :: IO (Decimal RoundHalfUp 3 Int)) :: IO (Decimal RoundHalfUp 1 Int)
-- 3.8
-- >>> roundDecimal <$> (3.760 :: IO (Decimal RoundHalfUp 3 Int)) :: IO (Decimal RoundHalfUp 1 Int)
-- 3.8
-- >>> roundDecimal <$> (-3.740 :: IO (Decimal RoundHalfUp 3 Int)) :: IO (Decimal RoundHalfUp 1 Int)
-- -3.7
-- >>> roundDecimal <$> (-3.749 :: IO (Decimal RoundHalfUp 3 Int)) :: IO (Decimal RoundHalfUp 1 Int)
-- -3.7
-- >>> roundDecimal <$> (-3.750 :: IO (Decimal RoundHalfUp 3 Int)) :: IO (Decimal RoundHalfUp 1 Int)
-- -3.7
-- >>> roundDecimal <$> (-3.751 :: IO (Decimal RoundHalfUp 3 Int)) :: IO (Decimal RoundHalfUp 1 Int)
-- -3.8
-- >>> roundDecimal <$> (-3.760 :: IO (Decimal RoundHalfUp 3 Int)) :: IO (Decimal RoundHalfUp 1 Int)
-- -3.8
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

-- | [Round half down](https://en.wikipedia.org/wiki/Rounding#Round_half_down): A very common
-- rounding strategy:
--
-- >>> :set -XDataKinds
-- >>> roundDecimal <$> (3.740 :: IO (Decimal RoundHalfDown 3 Int)) :: IO (Decimal RoundHalfDown 1 Int)
-- 3.7
-- >>> roundDecimal <$> (3.749 :: IO (Decimal RoundHalfDown 3 Int)) :: IO (Decimal RoundHalfDown 1 Int)
-- 3.7
-- >>> roundDecimal <$> (3.750 :: IO (Decimal RoundHalfDown 3 Int)) :: IO (Decimal RoundHalfDown 1 Int)
-- 3.7
-- >>> roundDecimal <$> (3.751 :: IO (Decimal RoundHalfDown 3 Int)) :: IO (Decimal RoundHalfDown 1 Int)
-- 3.8
-- >>> roundDecimal <$> (3.760 :: IO (Decimal RoundHalfDown 3 Int)) :: IO (Decimal RoundHalfDown 1 Int)
-- 3.8
-- >>> roundDecimal <$> (-3.740 :: IO (Decimal RoundHalfDown 3 Int)) :: IO (Decimal RoundHalfDown 1 Int)
-- -3.7
-- >>> roundDecimal <$> (-3.749 :: IO (Decimal RoundHalfDown 3 Int)) :: IO (Decimal RoundHalfDown 1 Int)
-- -3.7
-- >>> roundDecimal <$> (-3.750 :: IO (Decimal RoundHalfDown 3 Int)) :: IO (Decimal RoundHalfDown 1 Int)
-- -3.8
-- >>> roundDecimal <$> (-3.751 :: IO (Decimal RoundHalfDown 3 Int)) :: IO (Decimal RoundHalfDown 1 Int)
-- -3.8
-- >>> roundDecimal <$> (-3.760 :: IO (Decimal RoundHalfDown 3 Int)) :: IO (Decimal RoundHalfDown 1 Int)
-- -3.8
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

-- | [Round half even](https://en.wikipedia.org/wiki/Rounding#Round_half_to_even): if the
-- fractional part of x is 0.5, then y is the even integer nearest to x.
--
-- >>> :set -XDataKinds
-- >>> roundDecimal <$> (3.650 :: IO (Decimal RoundHalfEven 3 Int)) :: IO (Decimal RoundHalfEven 1 Int)
-- 3.6
-- >>> roundDecimal <$> (3.740 :: IO (Decimal RoundHalfEven 3 Int)) :: IO (Decimal RoundHalfEven 1 Int)
-- 3.7
-- >>> roundDecimal <$> (3.749 :: IO (Decimal RoundHalfEven 3 Int)) :: IO (Decimal RoundHalfEven 1 Int)
-- 3.7
-- >>> roundDecimal <$> (3.750 :: IO (Decimal RoundHalfEven 3 Int)) :: IO (Decimal RoundHalfEven 1 Int)
-- 3.8
-- >>> roundDecimal <$> (3.751 :: IO (Decimal RoundHalfEven 3 Int)) :: IO (Decimal RoundHalfEven 1 Int)
-- 3.8
-- >>> roundDecimal <$> (3.760 :: IO (Decimal RoundHalfEven 3 Int)) :: IO (Decimal RoundHalfEven 1 Int)
-- 3.8
-- >>> roundDecimal <$> (-3.650 :: IO (Decimal RoundHalfEven 3 Int)) :: IO (Decimal RoundHalfEven 1 Int)
-- -3.6
-- >>> roundDecimal <$> (-3.740 :: IO (Decimal RoundHalfEven 3 Int)) :: IO (Decimal RoundHalfEven 1 Int)
-- -3.7
-- >>> roundDecimal <$> (-3.749 :: IO (Decimal RoundHalfEven 3 Int)) :: IO (Decimal RoundHalfEven 1 Int)
-- -3.7
-- >>> roundDecimal <$> (-3.750 :: IO (Decimal RoundHalfEven 3 Int)) :: IO (Decimal RoundHalfEven 1 Int)
-- -3.8
-- >>> roundDecimal <$> (-3.751 :: IO (Decimal RoundHalfEven 3 Int)) :: IO (Decimal RoundHalfEven 1 Int)
-- -3.8
-- >>> roundDecimal <$> (-3.760 :: IO (Decimal RoundHalfEven 3 Int)) :: IO (Decimal RoundHalfEven 1 Int)
-- -3.8
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

-- | [Round down](https://en.wikipedia.org/wiki/Rounding#Rounding_down): Round towards minus infinity:
--
-- >>> :set -XDataKinds
-- >>> roundDecimal <$> (3.65 :: IO (Decimal RoundDown 2 Int)) :: IO (Decimal RoundDown 1 Int)
-- 3.6
-- >>> roundDecimal <$> (3.75 :: IO (Decimal RoundDown 2 Int)) :: IO (Decimal RoundDown 1 Int)
-- 3.7
-- >>> roundDecimal <$> (3.89 :: IO (Decimal RoundDown 2 Int)) :: IO (Decimal RoundDown 1 Int)
-- 3.8
-- >>> roundDecimal <$> (-3.65 :: IO (Decimal RoundDown 2 Int)) :: IO (Decimal RoundDown 1 Int)
-- -3.7
--
-- @since 0.2.0
data RoundDown

-- | Synonym for round down
--
-- @since 0.2.0
type Floor = RoundDown

instance Round RoundDown Integer where
  roundDecimal = roundDown
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

-- | [Round towards zero](https://en.wikipedia.org/wiki/Rounding#Round_towards_zero): drop
-- the fractional digits, regardless of the sign.
--
-- >>> :set -XDataKinds
-- >>> roundDecimal <$> (3.65 :: IO (Decimal Truncate 2 Int)) :: IO (Decimal Truncate 1 Int)
-- 3.6
-- >>> roundDecimal <$> (3.75 :: IO (Decimal Truncate 2 Int)) :: IO (Decimal Truncate 1 Int)
-- 3.7
-- >>> roundDecimal <$> (3.89 :: IO (Decimal Truncate 2 Int)) :: IO (Decimal Truncate 1 Int)
-- 3.8
-- >>> roundDecimal <$> (-3.65 :: IO (Decimal Truncate 2 Int)) :: IO (Decimal Truncate 1 Int)
-- -3.6
--
-- @since 0.2.0
data RoundToZero


-- | Synonym for `RoundToZero`
--
-- @since 0.1.0
type Truncate = RoundToZero

instance Round RoundToZero Integer where
  roundDecimal = roundRoundToZero
instance Round RoundToZero Int where
  roundDecimal = roundRoundToZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundToZero Int8 where
  roundDecimal = roundRoundToZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundToZero Int16 where
  roundDecimal = roundRoundToZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundToZero Int32 where
  roundDecimal = roundRoundToZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundToZero Int64 where
  roundDecimal = roundRoundToZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundToZero Word where
  roundDecimal = roundRoundToZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundToZero Word8 where
  roundDecimal = roundRoundToZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundToZero Word16 where
  roundDecimal = roundRoundToZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundToZero Word32 where
  roundDecimal = roundRoundToZero
  {-# INLINABLE roundDecimal #-}
instance Round RoundToZero Word64 where
  roundDecimal = roundRoundToZero
  {-# INLINABLE roundDecimal #-}

roundRoundToZero :: forall r n k p . (Integral p, KnownNat k) => Decimal r (n + k) p -> Decimal r n p
roundRoundToZero (Decimal x) = Decimal (quot x (10 ^ k))
  where
    k = fromIntegral (natVal (Proxy :: Proxy k)) :: Int
{-# INLINABLE roundRoundToZero #-}

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
-- >>> mapM fromIntegral [1, 20, 300 :: Int] :: Either SomeException [Decimal RoundHalfUp 2 Int]
-- Right [1.00,20.00,300.00]
--
-- @since 0.1.0
decimalList :: Integral p => [p] -> [Decimal r s p]
decimalList = coerce


-- | Sum a list of decimal numbers
--
-- >>> :set -XDataKinds
-- >>> mapM fromRational [1.1, 20.02, 300.003] >>= sumDecimalBounded :: IO (Decimal RoundHalfUp 3 Int)
-- 321.123
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
-- >>> xs <- mapM fromRational [1.1, 20.02, 300.003] :: IO [Decimal RoundHalfUp 4 Int]
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
toScientific :: (Integral p, KnownNat s) => Decimal r s p -> Scientific
toScientific dec = scientific (toInteger (unwrapDecimal dec)) (fromInteger (negate (getScale dec)))

-- | Convert Scientific to Decimal without loss of precision. Will return `Left` `Underflow` if
-- `Scientific` has too many decimal places, more than `Decimal` scaling is capable to handle.
--
-- @since 0.1.0
fromScientific :: forall m r s . (MonadThrow m, KnownNat s) => Scientific -> m (Decimal r s Integer)
fromScientific num
  | point10 > s = throwM Underflow
  | otherwise = pure (Decimal (coefficient num * 10 ^ (s - point10)))
  where
      s = natVal (Proxy :: Proxy s)
      point10 = toInteger (negate (base10Exponent num))

-- | Convert from Scientific to Decimal while checking for Overflow/Underflow
--
-- @since 0.1.0
fromScientificBounded ::
     forall m r s p. (MonadThrow m, Integral p, Bounded p, KnownNat s)
  => Scientific
  -> m (Decimal r s p)
fromScientificBounded num = do
  Decimal integer :: Decimal r s Integer <- fromScientific num
  Decimal <$> fromIntegerBounded integer


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
-- >>> toFixed <$> (3.65 :: IO (Decimal RoundDown 2 Int)) :: IO (Fixed E2)
-- 3.65
-- >>> toFixed $ fromFixed (123.45 :: Fixed E2) :: Fixed E2
-- 123.45
--
-- @since 0.2.0
toFixed :: (s ~ FixedScale e, Integral p) => Decimal r s p -> Fixed e
toFixed = MkFixed . toInteger . unwrapDecimal

-- | Convert a `Fixed` to a `Decimal` with the exactly same precision
--
-- >>> fromFixed (123.45 :: Fixed E2)
-- 123.45
--
-- @since 0.2.0
fromFixed :: s ~ FixedScale e => Fixed e -> Decimal r s Integer
fromFixed = coerce

-- | Convert a `Fixed` to a decimal backed by a bounded integral with the exactly same
-- precision
--
-- >>> fromFixedBounded (123.458 :: Fixed E3) :: IO (Decimal RoundToZero 3 Int)
-- 123.458
-- >>> fromFixedBounded (123.458 :: Fixed E3) :: IO (Decimal RoundToZero 3 Int8)
-- *** Exception: arithmetic overflow
-- >>> fromFixedBounded (-123.458 :: Fixed E3) :: IO (Decimal RoundToZero 3 Word)
-- *** Exception: arithmetic underflow
--
-- @since 0.2.0
fromFixedBounded ::
     (s ~ FixedScale e, MonadThrow m, Integral p, Bounded p)
  => Fixed e
  -> m (Decimal r s p)
fromFixedBounded = fromIntegerDecimalBounded . fromFixed
