{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE NegativeLiterals           #-}
module Numeric.Decimal
  ( Decimal64
  , RoundHalfUp
  , RoundFloor
  , Truncate
  , module Numeric.Decimal.Internal
  -- * Operations
  , decimalList
  , sumDecimal
  , productDecimal
  -- * Conversion
  , toScientific
  , fromScientific
  , fromScientificBounded
  ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch
import           Data.Coerce
import           Data.Int
import           Data.Proxy
import           Data.Scientific
import           GHC.TypeLits
import           Numeric.Decimal.Internal

-- | Most common Decimal type backed by `Int64` and standard rounding
type Decimal64 s = Decimal RoundHalfUp s Int64

data RoundHalfUp

instance Round RoundHalfUp where
  roundDecimal :: forall r n k p . (Integral p, KnownNat k) => Decimal r (n + k) p -> Decimal r n p
  roundDecimal (Decimal x)
    | k == 0               = Decimal x
    | r < 5 * 10 ^ (k - 1) = Decimal q
    | otherwise            = Decimal (q + 1)
    where
      k = fromIntegral (natVal (Proxy :: Proxy k)) :: Int
      (q, r) = quotRem x (10 ^ k)
  {-# INLINABLE roundDecimal #-}

data RoundFloor

instance Round RoundFloor where
  roundDecimal :: forall r n k p . (Integral p, KnownNat k) => Decimal r (n + k) p -> Decimal r n p
  roundDecimal (Decimal x)
    | x >= 0 || r == 0 = Decimal q
    | otherwise = Decimal (q - 1)
    where
      k = fromIntegral (natVal (Proxy :: Proxy k)) :: Int
      (q, r) = quotRem x (10 ^ k)
  {-# INLINABLE roundDecimal #-}

data Truncate

instance Round Truncate where
  roundDecimal :: forall r n k p . (Integral p, KnownNat k) => Decimal r (n + k) p -> Decimal r n p
  roundDecimal (Decimal x) = Decimal (quot x (10 ^ k))
    where
      k = fromIntegral (natVal (Proxy :: Proxy k)) :: Int
  {-# INLINABLE roundDecimal #-}

-- | /O(1)/ - Conversion of a list.
--
-- __Note__: It doesn't do any scaling, eg:
--
-- >>> decimalList [1,20,300] :: [Decimal RoundHalfUp 2 Int]
-- [0.01,0.20,3.00]
--
-- If scaling is what you need use `fromIntegral` instead:
--
-- >>> mapM fromIntegral ([1,20,300] :: [Int]) :: Either SomeException [Decimal RoundHalfUp 2 Int]
-- Right [1.00,20.00,300.00]
--
decimalList :: Integral p => [p] -> [Decimal r s p]
decimalList = coerce


-- | Sum a list of decimal numbers
sumDecimal ::
     (MonadThrow m, Foldable f, Eq p, Ord p, Num p, Bounded p)
  => f (Decimal r s p)
  -> m (Decimal r s p)
sumDecimal = foldM plusDecimal (Decimal 0)
{-# INLINABLE sumDecimal #-}

-- | Multiply all decimal numbers in the list while doing rounding.
productDecimal ::
     (MonadThrow m, Foldable f, KnownNat s, Round r, Integral p, Bounded p)
  => f (Decimal r s p)
  -> m (Decimal r s p)
productDecimal = foldM timesDecimalRounded (fromNum 1)
{-# INLINABLE productDecimal #-}



---- Scientific

-- | Convert Decimal to Scientific
toScientific :: (Integral p, KnownNat s) => Decimal r s p -> Scientific
toScientific dec = scientific (toInteger (unwrapDecimal dec)) (negate (getScale dec))

-- | Convert Scientific to Decimal without loss of precision. Will return `Left` `Underflow` if
-- `Scientific` has too many decimal places, more than `Decimal` scaling is capable to handle.
fromScientific :: forall m r s . (MonadThrow m, KnownNat s) => Scientific -> m (Decimal r s Integer)
fromScientific num
  | point10 > s = throwM Underflow
  | otherwise = pure (Decimal (coefficient num * 10 ^ (s - point10)))
  where
      s = natVal (Proxy :: Proxy s)
      point10 = toInteger (negate (base10Exponent num))

-- | Convert from Scientific to Decimal while checking for Overflow/Underflow
fromScientificBounded ::
     forall m r s p. (MonadThrow m, Integral p, Bounded p, KnownNat s)
  => Scientific
  -> m (Decimal r s p)
fromScientificBounded num = do
  Decimal integer :: Decimal r s Integer <- fromScientific num
  Decimal <$> fromIntegerBounded integer
