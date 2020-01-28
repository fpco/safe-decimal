{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Numeric.Decimal.Internal
  ( Decimal(..)
  , Round(..)
  , wrapDecimal
  , unwrapDecimal
  , splitDecimal
  , getScale
  , fromIntegerDecimal
  , fromIntegralDecimalBounded
  , scaleUp
  , scaleUpBounded
  , castRounding
  , parseDecimalBounded
  -- * Algebra
  , decimalNumerator
  , decimalDenominator
  , plusDecimal
  , minusDecimal
  , timesDecimal
  , signumDecimal
  , signumDecimalBounded
  , timesDecimalBounded
  , timesDecimalRounded
  , timesDecimalWithoutLoss
  , divideDecimal
  , divideDecimalWithoutLoss
  , quotRemBounded
  , quotRemDecimalBounded
  , fromIntegerDecimalBounded
  , toRationalDecimal
  , fromRationalDecimal
  , fromRationalDecimalBounded
  , fromRationalDecimalRounded
  , bindM2Decimal
  , bindM2
  -- * Bounded
  , plusBounded
  , minusBounded
  , timesBounded
  , fromIntegerBounded
  , fromIntegerScaleBounded
  , divBounded
  , quotBounded
  -- ** Exceptions
  , MonadThrow(..)
  , ArithException(..)
  , SomeException
  ) where

import Control.Applicative
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Catch
import Data.Char
import Data.Coerce
import Data.Foldable as F
import Data.Int
import Data.List
import Data.Proxy
import Data.Ratio
import Data.Word
import GHC.Generics (Generic)
import GHC.TypeLits
import Text.Printf

-- | Decimal number with custom precision (@p@) and type level scaling (@s@) parameter (i.e. number
-- of digits after the decimal point). As well as the rounding (@r@) strategy to use.
newtype Decimal r (s :: Nat) p = Decimal p
  deriving (Enum, Ord, Eq, NFData, Functor, Generic)

instance Applicative (Decimal r s) where
  pure = Decimal
  {-# INLINABLE pure #-}
  (<*>) (Decimal f) (Decimal x) = Decimal (f x)
  {-# INLINABLE (<*>) #-}


-- | Rounding strategy to be used with decimal numbers.
--
-- @since 0.1.0
class Integral p => Round r p where

  -- | Reduce the scale of a number by @k@ decimal places using rounding strategy @r@
  --
  -- @since 0.1.0
  roundDecimal :: KnownNat k => Decimal r (n + k) p -> Decimal r n p

-- | Change the rounding strategy of a `Decimal`
--
-- @since 0.2.0
castRounding :: Round r' p => Decimal r s p -> Decimal r' s p
castRounding = coerce


data PrecisionLoss = PrecisionLoss !Rational !Integer
  deriving Eq

instance Show PrecisionLoss where
  show (PrecisionLoss r s) = "PrecisionLoss (" ++ show r ++ ") to " ++ show s ++ " decimal spaces"

instance Exception PrecisionLoss

-- | Get the scale of a `Decimal`. Argument is not evaluated.
--
-- >>> import Numeric.Decimal
-- >>> d <- 36 :: IO (Decimal RoundHalfUp 5 Int)
-- >>> d
-- 36.00000
-- >>> getScale d
-- 5
--
-- @since 0.1.0
getScale :: forall r s p . KnownNat s => Decimal r s p -> Integer
getScale _ = natVal (Proxy :: Proxy s)


-- | Increase the precision of a `Decimal`, use `roundDecimal` if inverse is desired.
--
-- >>> import Numeric.Decimal
-- >>> d2 <- 1.65 :: IO (Decimal RoundHalfUp 2 Integer)
-- >>> d2
-- 1.65
-- >>> scaleUp d2 :: Decimal RoundHalfUp 50 Integer
-- 1.65000000000000000000000000000000000000000000000000
--
-- @since 0.2.0
scaleUp ::
     forall k r n. KnownNat k
  => Decimal r n Integer
  -> Decimal r (n + k) Integer
scaleUp (Decimal d) = Decimal (d * (10 ^ natVal (Proxy :: Proxy k)))

-- | Increase the precision of a `Decimal` backed by a bounded type, use `roundDecimal` if
-- inverse is desired.
--
-- >>> import Numeric.Decimal
-- >>> d2 <- 1.65 :: IO (Decimal RoundHalfUp 2 Int16)
-- >>> scaleUpBounded d2 :: IO (Decimal RoundHalfUp 3 Int16)
-- 1.650
-- >>> scaleUpBounded d2 :: IO (Decimal RoundHalfUp 4 Int16)
-- 1.6500
-- >>> scaleUpBounded d2 :: IO (Decimal RoundHalfUp 5 Int16)
-- *** Exception: arithmetic overflow
--
-- @since 0.1.1
scaleUpBounded ::
     forall k r n p m. (MonadThrow m, Integral p, Bounded p, KnownNat k)
  => Decimal r n p
  -> m (Decimal r (n + k) p)
scaleUpBounded (Decimal d) = do
  i <- fromIntegerBounded (10 ^ natVal (Proxy :: Proxy k))
  Decimal <$> timesBounded d i

-- | Split the number at the decimal point, i.e. whole number and the fraction
--
-- >>> import Numeric.Decimal
-- >>> splitDecimal <$> (12.34 :: IO (Decimal RoundHalfUp 2 Int))
-- (12,34)
--
-- @since 0.1.0
splitDecimal :: (Integral p, KnownNat s) => Decimal r s p -> (p, p)
splitDecimal d@(Decimal v) = v `quotRem` (10 ^ getScale d)

-- | Wrap an `Integral` as a `Decimal`. No scaling will be done.
--
-- >>> import Numeric.Decimal
-- >>> wrapDecimal 1234 :: Decimal RoundHalfUp 4 Int
-- 0.1234
-- >>> wrapDecimal 1234 :: Decimal RoundHalfUp 2 Int
-- 12.34
--
-- @since 0.1.0
wrapDecimal :: Integral p => p -> Decimal r s p
wrapDecimal = Decimal

-- | Get out the underlying representation for the decimal number. No scaling will be done.
--
-- >>> import Numeric.Decimal
-- >>> unwrapDecimal (wrapDecimal 1234 :: Decimal RoundHalfUp 4 Int)
-- 1234
--
-- @since 0.1.0
unwrapDecimal :: Decimal r s p -> p
unwrapDecimal (Decimal p) = p

-- | Convert an `Integer` while performing the necessary scaling
--
-- >>> import Numeric.Decimal
-- >>> fromIntegerDecimal 1234 :: Decimal RoundHalfUp 4 Integer
-- 1234.0000
--
-- @since 0.2.0
fromIntegerDecimal :: forall r s . KnownNat s => Integer -> Decimal r s Integer
fromIntegerDecimal x = Decimal (x * (10 ^ s))
  where
    s = natVal (Proxy :: Proxy s)
{-# INLINABLE fromIntegerDecimal #-}

-- | Convert a bounded integeral into a decimal, while performing the necessary scaling
--
-- >>> import Numeric.Decimal
-- >>> fromIntegralDecimalBounded 1234 :: IO (Decimal RoundHalfUp 4 Int)
-- 1234.0000
-- >>> fromIntegralDecimalBounded 1234 :: IO (Decimal RoundHalfUp 4 Int16)
-- *** Exception: arithmetic overflow
--
-- @since 0.2.0
fromIntegralDecimalBounded ::
     (Integral p, Bounded p, KnownNat s, MonadThrow m) => p -> m (Decimal r s p)
fromIntegralDecimalBounded = fromIntegerDecimalBounded . fromIntegerDecimal . toInteger
{-# INLINABLE fromIntegralDecimalBounded #-}


bindM2Decimal ::
     Monad m
  => (p1 -> p2 -> m p)
  -> m (Decimal r1 s1 p1)
  -> m (Decimal r2 s2 p2)
  -> m (Decimal r s p)
bindM2Decimal f dx dy = do
  Decimal x <- dx
  Decimal y <- dy
  Decimal <$> f x y
{-# INLINABLE bindM2Decimal #-}


bindM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f mx my = do
  x <- mx
  y <- my
  f x y
{-# INLINABLE bindM2 #-}


instance Bounded p => Bounded (Decimal r s p) where
  minBound = Decimal minBound
  maxBound = Decimal maxBound

-----------------------------------
-- Integer instances --------------
-----------------------------------

instance (MonadThrow m, KnownNat s) => Fractional (m (Decimal r s Integer)) where
  (/) = bindM2 divideDecimal
  {-# INLINABLE (/) #-}
  fromRational = fromRationalDecimal
  {-# INLINABLE fromRational #-}

instance (MonadThrow m, KnownNat s) => Num (m (Decimal r s Integer)) where
  (+) = liftA2 (liftA2 (+))
  {-# INLINABLE (+) #-}
  (-) = liftA2 (liftA2 (-))
  {-# INLINABLE (-) #-}
  (*) = bindM2 timesDecimalWithoutLoss
  {-# INLINABLE (*) #-}
  signum = fmap signumDecimal
  {-# INLINABLE signum #-}
  abs = fmap (fmap abs)
  {-# INLINABLE abs #-}
  fromInteger = pure . fromIntegerDecimal
  {-# INLINABLE fromInteger #-}


-----------------------------------
-- Bounded Integral instances -----
-----------------------------------


instance (MonadThrow m, KnownNat s) => Num (m (Decimal r s Int)) where
  (+) = bindM2 plusDecimal
  {-# INLINABLE (+) #-}
  (-) = bindM2 minusDecimal
  {-# INLINABLE (-) #-}
  (*) = bindM2 timesDecimalWithoutLoss
  {-# INLINABLE (*) #-}
  signum = (>>= signumDecimalBounded)
  {-# INLINABLE signum #-}
  abs = fmap (fmap abs)
  {-# INLINABLE abs #-}
  fromInteger = fmap Decimal . fromIntegerScaleBounded (Proxy :: Proxy s)
  {-# INLINABLE fromInteger #-}

instance (MonadThrow m, KnownNat s) => Num (m (Decimal r s Int8)) where
  (+) = bindM2 plusDecimal
  {-# INLINABLE (+) #-}
  (-) = bindM2 minusDecimal
  {-# INLINABLE (-) #-}
  (*) = bindM2 timesDecimalWithoutLoss
  {-# INLINABLE (*) #-}
  signum = (>>= signumDecimalBounded)
  {-# INLINABLE signum #-}
  abs = fmap (fmap abs)
  {-# INLINABLE abs #-}
  fromInteger = fmap Decimal . fromIntegerScaleBounded (Proxy :: Proxy s)
  {-# INLINABLE fromInteger #-}

instance (MonadThrow m, KnownNat s) => Num (m (Decimal r s Int16)) where
  (+) = bindM2 plusDecimal
  {-# INLINABLE (+) #-}
  (-) = bindM2 minusDecimal
  {-# INLINABLE (-) #-}
  (*) = bindM2 timesDecimalWithoutLoss
  {-# INLINABLE (*) #-}
  signum = (>>= signumDecimalBounded)
  {-# INLINABLE signum #-}
  abs = fmap (fmap abs)
  {-# INLINABLE abs #-}
  fromInteger = fmap Decimal . fromIntegerScaleBounded (Proxy :: Proxy s)
  {-# INLINABLE fromInteger #-}

instance (MonadThrow m, KnownNat s) => Num (m (Decimal r s Int32)) where
  (+) = bindM2 plusDecimal
  {-# INLINABLE (+) #-}
  (-) = bindM2 minusDecimal
  {-# INLINABLE (-) #-}
  (*) = bindM2 timesDecimalWithoutLoss
  {-# INLINABLE (*) #-}
  signum = (>>= signumDecimalBounded)
  {-# INLINABLE signum #-}
  abs = fmap (fmap abs)
  {-# INLINABLE abs #-}
  fromInteger = fmap Decimal . fromIntegerScaleBounded (Proxy :: Proxy s)
  {-# INLINABLE fromInteger #-}

instance (MonadThrow m, KnownNat s) => Num (m (Decimal r s Int64)) where
  (+) = bindM2 plusDecimal
  {-# INLINABLE (+) #-}
  (-) = bindM2 minusDecimal
  {-# INLINABLE (-) #-}
  (*) = bindM2 timesDecimalWithoutLoss
  {-# INLINABLE (*) #-}
  signum = (>>= signumDecimalBounded)
  {-# INLINABLE signum #-}
  abs = fmap (fmap abs)
  {-# INLINABLE abs #-}
  fromInteger = fmap Decimal . fromIntegerScaleBounded (Proxy :: Proxy s)
  {-# INLINABLE fromInteger #-}

instance (MonadThrow m, KnownNat s) => Num (m (Decimal r s Word)) where
  (+) = bindM2 plusDecimal
  {-# INLINABLE (+) #-}
  (-) = bindM2 minusDecimal
  {-# INLINABLE (-) #-}
  (*) = bindM2 timesDecimalWithoutLoss
  {-# INLINABLE (*) #-}
  signum = (>>= signumDecimalBounded)
  {-# INLINABLE signum #-}
  abs = id
  {-# INLINABLE abs #-}
  fromInteger = fmap Decimal . fromIntegerScaleBounded (Proxy :: Proxy s)
  {-# INLINABLE fromInteger #-}

instance (MonadThrow m, KnownNat s) => Num (m (Decimal r s Word8)) where
  (+) = bindM2 plusDecimal
  {-# INLINABLE (+) #-}
  (-) = bindM2 minusDecimal
  {-# INLINABLE (-) #-}
  (*) = bindM2 timesDecimalWithoutLoss
  {-# INLINABLE (*) #-}
  signum = (>>= signumDecimalBounded)
  {-# INLINABLE signum #-}
  abs = id
  {-# INLINABLE abs #-}
  fromInteger = fmap Decimal . fromIntegerScaleBounded (Proxy :: Proxy s)
  {-# INLINABLE fromInteger #-}

instance (MonadThrow m, KnownNat s) => Num (m (Decimal r s Word16)) where
  (+) = bindM2 plusDecimal
  {-# INLINABLE (+) #-}
  (-) = bindM2 minusDecimal
  {-# INLINABLE (-) #-}
  (*) = bindM2 timesDecimalWithoutLoss
  {-# INLINABLE (*) #-}
  signum = (>>= signumDecimalBounded)
  {-# INLINABLE signum #-}
  abs = id
  {-# INLINABLE abs #-}
  fromInteger = fmap Decimal . fromIntegerScaleBounded (Proxy :: Proxy s)
  {-# INLINABLE fromInteger #-}

instance (MonadThrow m, KnownNat s) => Num (m (Decimal r s Word32)) where
  (+) = bindM2 plusDecimal
  {-# INLINABLE (+) #-}
  (-) = bindM2 minusDecimal
  {-# INLINABLE (-) #-}
  (*) = bindM2 timesDecimalWithoutLoss
  {-# INLINABLE (*) #-}
  signum = (>>= signumDecimalBounded)
  {-# INLINABLE signum #-}
  abs = id
  {-# INLINABLE abs #-}
  fromInteger = fmap Decimal . fromIntegerScaleBounded (Proxy :: Proxy s)
  {-# INLINABLE fromInteger #-}

instance (MonadThrow m, KnownNat s) => Num (m (Decimal r s Word64)) where
  (+) = bindM2 plusDecimal
  {-# INLINABLE (+) #-}
  (-) = bindM2 minusDecimal
  {-# INLINABLE (-) #-}
  (*) = bindM2 timesDecimalWithoutLoss
  {-# INLINABLE (*) #-}
  signum = (>>= signumDecimalBounded)
  {-# INLINABLE signum #-}
  abs = id
  {-# INLINABLE abs #-}
  fromInteger = fmap Decimal . fromIntegerScaleBounded (Proxy :: Proxy s)
  {-# INLINABLE fromInteger #-}

instance (MonadThrow m, KnownNat s) => Fractional (m (Decimal r s Int)) where
  (/) = bindM2 divideDecimal
  {-# INLINABLE (/) #-}
  fromRational r = fromRationalDecimal r >>= fromIntegerDecimalBounded
  {-# INLINABLE fromRational #-}

instance (MonadThrow m, KnownNat s) => Fractional (m (Decimal r s Int8)) where
  (/) = bindM2 divideDecimal
  {-# INLINABLE (/) #-}
  fromRational r = fromRationalDecimal r >>= fromIntegerDecimalBounded
  {-# INLINABLE fromRational #-}

instance (MonadThrow m, KnownNat s) => Fractional (m (Decimal r s Int16)) where
  (/) = bindM2 divideDecimal
  {-# INLINABLE (/) #-}
  fromRational r = fromRationalDecimal r >>= fromIntegerDecimalBounded
  {-# INLINABLE fromRational #-}

instance (MonadThrow m, KnownNat s) => Fractional (m (Decimal r s Int32)) where
  (/) = bindM2 divideDecimal
  {-# INLINABLE (/) #-}
  fromRational r = fromRationalDecimal r >>= fromIntegerDecimalBounded
  {-# INLINABLE fromRational #-}


instance (MonadThrow m, KnownNat s) => Fractional (m (Decimal r s Int64)) where
  (/) = bindM2 divideDecimal
  {-# INLINABLE (/) #-}
  fromRational r = fromRationalDecimal r >>= fromIntegerDecimalBounded
  {-# INLINABLE fromRational #-}

instance (MonadThrow m, KnownNat s) => Fractional (m (Decimal r s Word)) where
  (/) = bindM2 divideDecimal
  {-# INLINABLE (/) #-}
  fromRational r = fromRationalDecimal r >>= fromIntegerDecimalBounded
  {-# INLINABLE fromRational #-}

instance (MonadThrow m, KnownNat s) => Fractional (m (Decimal r s Word8)) where
  (/) = bindM2 divideDecimal
  {-# INLINABLE (/) #-}
  fromRational r = fromRationalDecimal r >>= fromIntegerDecimalBounded
  {-# INLINABLE fromRational #-}

instance (MonadThrow m, KnownNat s) => Fractional (m (Decimal r s Word16)) where
  (/) = bindM2 divideDecimal
  {-# INLINABLE (/) #-}
  fromRational r = fromRationalDecimal r >>= fromIntegerDecimalBounded
  {-# INLINABLE fromRational #-}

instance (MonadThrow m, KnownNat s) => Fractional (m (Decimal r s Word32)) where
  (/) = bindM2 divideDecimal
  {-# INLINABLE (/) #-}
  fromRational r = fromRationalDecimal r >>= fromIntegerDecimalBounded
  {-# INLINABLE fromRational #-}

instance (MonadThrow m, KnownNat s) => Fractional (m (Decimal r s Word64)) where
  (/) = bindM2 divideDecimal
  {-# INLINABLE (/) #-}
  fromRational r = fromRationalDecimal r >>= fromIntegerDecimalBounded
  {-# INLINABLE fromRational #-}

divideDecimal ::
     (MonadThrow m, Fractional (m (Decimal r s p)), Integral p, Integral p)
  => Decimal r s p
  -> Decimal r s p
  -> m (Decimal r s p)
divideDecimal (Decimal x) (Decimal y)
  | y == 0 = throwM DivideByZero
  | otherwise = fromRational (toInteger x % toInteger y)
{-# INLINABLE divideDecimal #-}


-----------------------------------
-- Helper functions ---------------
-----------------------------------

-- | Add two bounded numbers while checking for `Overflow`/`Underflow`
plusBounded :: (MonadThrow m, Eq a, Ord a, Num a, Bounded a) => a -> a -> m a
plusBounded x y
  | sameSig && sigX ==  1 && x > maxBound - y = throwM Overflow
  | sameSig && sigX == -1 && x < minBound - y = throwM Underflow
  | otherwise = pure (x + y)
  where
    sigX = signum x
    sigY = signum y
    sameSig = sigX == sigY
{-# INLINABLE plusBounded #-}

-- | Subtract two bounded numbers while checking for `Overflow`/`Underflow`
minusBounded :: (MonadThrow m, Eq a, Ord a, Num a, Bounded a) => a -> a -> m a
minusBounded x y
  | sigY == -1 && x > maxBound + y = throwM Overflow
  | sigY ==  1 && x < minBound + y = throwM Underflow
  | otherwise = pure (x - y)
  where sigY = signum y
{-# INLINABLE minusBounded #-}

-- | Divide two decimal numbers while checking for `Overflow` and `DivideByZero`
divBounded :: (MonadThrow m, Integral a, Bounded a) => a -> a -> m a
divBounded x y
  | y == 0 = throwM DivideByZero
  | signum y == -1 && y == -1 && x == minBound = throwM Overflow
    ------------------- ^ Here we deal with special case overflow when (minBound * (-1))
  | otherwise = pure (x `div` y)
{-# INLINABLE divBounded #-}


-- | Divide two decimal numbers while checking for `Overflow` and `DivideByZero`
quotBounded :: (MonadThrow m, Integral a, Bounded a) => a -> a -> m a
quotBounded x y
  | y == 0 = throwM DivideByZero
  | sigY == -1 && y == -1 && x == minBound = throwM Overflow
    ------------------- ^ Here we deal with special case overflow when (minBound * (-1))
  | otherwise = pure (x `quot` y)
  where
    sigY = signum y -- Guard against wraparound in case of unsigned Word
{-# INLINABLE quotBounded #-}

-- | Divide two decimal numbers while checking for `Overflow` and `DivideByZero`
quotRemBounded :: (MonadThrow m, Integral a, Bounded a) => a -> a -> m (a, a)
quotRemBounded x y
  | y == 0 = throwM DivideByZero
  | sigY == -1 && y == -1 && x == minBound = throwM Overflow
  | otherwise = pure (x `quotRem` y)
  where
    sigY = signum y
{-# INLINABLE quotRemBounded #-}

quotRemDecimalBounded ::
     forall m r s p. (MonadThrow m, Integral p, Bounded p)
  => Decimal r s p
  -> Integer
  -> m (Decimal r s p, Decimal r s p)
quotRemDecimalBounded (Decimal raw) i
  | i < toInteger (minBound :: p) = throwM Underflow
  | i > toInteger (maxBound :: p) = throwM Overflow
  | otherwise = do
      (q, r) <- quotRemBounded raw $ fromInteger i
      pure (Decimal q, Decimal r)
{-# INLINABLE quotRemDecimalBounded #-}


-- | Multiply two decimal numbers while checking for `Overflow`
timesBounded :: (MonadThrow m, Integral a, Bounded a) => a -> a -> m a
timesBounded x y
  | sigY == -1 && y == -1 && x == minBound = throwM Overflow
  | signum x == -1 && x == -1 && y == minBound = throwM Overflow
  | sigY ==  1 && (minBoundQuotY > x || x > maxBoundQuotY) = eitherOverUnder
  | sigY == -1 && y /= -1 && (minBoundQuotY < x || x < maxBoundQuotY) = eitherOverUnder
  | otherwise = pure (x * y)
  where
    sigY = signum y
    maxBoundQuotY = maxBound `quot` y
    minBoundQuotY = minBound `quot` y
    eitherOverUnder = throwM $ if sigY == signum x then Overflow else Underflow
{-# INLINABLE timesBounded #-}


fromIntegerBounded ::
     forall m a. (MonadThrow m, Integral a, Bounded a)
  => Integer
  -> m a
fromIntegerBounded x
  | x > toInteger (maxBound :: a) = throwM Overflow
  | x < toInteger (minBound :: a) = throwM Underflow
  | otherwise = pure $ fromInteger x
{-# INLINABLE fromIntegerBounded #-}

fromIntegerScaleBounded ::
     forall m a s. (MonadThrow m, Integral a, Bounded a, KnownNat s)
  => Proxy s
  -> Integer
  -> m a
fromIntegerScaleBounded ps x
  | xs > toInteger (maxBound :: a) = throwM Overflow
  | xs < toInteger (minBound :: a) = throwM Underflow
  | otherwise = pure $ fromInteger xs
  where s = natVal ps
        xs = x * (10 ^ s)
{-# INLINABLE fromIntegerScaleBounded #-}


fromIntegerDecimalBounded ::
     forall m r s p. (MonadThrow m, Integral p, Bounded p)
  => Decimal r s Integer
  -> m (Decimal r s p)
fromIntegerDecimalBounded (Decimal x) = Decimal <$> fromIntegerBounded x
{-# INLINABLE fromIntegerDecimalBounded #-}


-- | Add two decimal numbers.
plusDecimal ::
     (MonadThrow m, Eq p, Ord p, Num p, Bounded p)
  => Decimal r s p
  -> Decimal r s p
  -> m (Decimal r s p)
plusDecimal (Decimal x) (Decimal y) = Decimal <$> plusBounded x y
{-# INLINABLE plusDecimal #-}

-- | Subtract two decimal numbers.
minusDecimal ::
     (MonadThrow m, Eq p, Ord p, Num p, Bounded p)
  => Decimal r s p
  -> Decimal r s p
  -> m (Decimal r s p)
minusDecimal (Decimal x) (Decimal y) = Decimal <$> minusBounded x y
{-# INLINABLE minusDecimal #-}

-- | Multiply two bounded decimal numbers, adjusting their scale at the type level as well.
timesDecimalBounded ::
     (MonadThrow m, Integral p, Bounded p)
  => Decimal r s1 p
  -> Decimal r s2 p
  -> m (Decimal r (s1 + s2) p)
timesDecimalBounded (Decimal x) (Decimal y) = Decimal <$> timesBounded x y
{-# INLINABLE timesDecimalBounded #-}

-- | Multiply two bounded decimal numbers, adjusting their scale at the type level as well.
timesDecimal ::
     Decimal r s1 Integer
  -> Decimal r s2 Integer
  -> Decimal r (s1 + s2) Integer
timesDecimal (Decimal x) (Decimal y) = Decimal (x * y)
{-# INLINABLE timesDecimal #-}


-- | Multiply two decimal numbers backed by `Integer`, while rounding the result according
-- to the rounding strategy.
timesDecimalRoundedInteger ::
     (KnownNat s, Round r Integer)
  => Decimal r s Integer
  -> Decimal r s Integer
  -> Decimal r s Integer
timesDecimalRoundedInteger dx dy = roundDecimal $ timesDecimal dx dy
{-# INLINABLE timesDecimalRoundedInteger #-}


-- | Multiply two decimal numbers, while rounding the result according to the rounding strategy.
timesDecimalRounded ::
     (MonadThrow m, KnownNat s, Round r Integer, Integral p, Bounded p)
  => Decimal r s p
  -> Decimal r s p
  -> m (Decimal r s p)
timesDecimalRounded dx dy =
  fromIntegerDecimalBounded $ timesDecimalRoundedInteger (fmap toInteger dx) (fmap toInteger dy)
{-# INLINABLE timesDecimalRounded #-}



-- | Multiply two decimal numbers that have the same scale, while throwing `PrecisionLoss`
-- whenever multiplication cannot be done without rounding.
--
-- @since 0.2.0
timesDecimalWithoutLoss ::
     forall r s p m. (Integral p, KnownNat s, MonadThrow m)
  => Decimal r s p
  -> Decimal r s p
  -> m (Decimal r s p)
timesDecimalWithoutLoss d1 (Decimal i2)
  | q /= toRational i =
    throwM $ PrecisionLoss (q * (1 % decimalDenominator d1)) $ getScale d1
  | otherwise = pure $ Decimal i
  where
    q = toRationalDecimal d1 * (toInteger i2 % 1)
    i = truncate q

-- | Divide two decimal numbers that have the same scale, while throwing `PrecisionLoss`
-- whenever division cannot be done without rounding.
--
-- @since 0.2.0
divideDecimalWithoutLoss ::
     forall r s p m. (Integral p, KnownNat s, MonadThrow m)
  => Decimal r s p
  -> Decimal r s p
  -> m (Decimal r s p)
divideDecimalWithoutLoss d1 (Decimal i2)
  | i2 == 0 = throwM DivideByZero
  | q /= toRational i = throwM $ PrecisionLoss (q * (1 % decimalDenominator d1)) $ getScale d1
  | otherwise = pure $ Decimal i
  where
    q = (decimalNumerator d1 * decimalDenominator d1) % toInteger i2
    i = truncate q


-- | Get the numerator. Same as `toInteger . unwrapDecimal`
--
-- @since 0.2.0
decimalNumerator :: Integral p => Decimal r s p -> Integer
decimalNumerator (Decimal i) = toInteger i

-- | Get the decimal denominator. Always will be a multiple of @10@. Does not evaluate the
-- argument.
--
-- @since 0.2.0
decimalDenominator :: KnownNat s => Decimal r s p -> Integer
decimalDenominator d = 10 ^ getScale d


toRationalDecimalInteger :: forall r s . KnownNat s => Decimal r s Integer -> Rational
toRationalDecimalInteger (Decimal p) = p % (10 ^ natVal (Proxy :: Proxy s))
{-# INLINABLE toRationalDecimalInteger #-}

-- | Convert a decimal to a Rational
--
-- @since 0.2.0
toRationalDecimal ::
     (KnownNat s, Integral p) => Decimal r s p -> Rational
toRationalDecimal d = toRationalDecimalInteger (toInteger <$> d)
{-# INLINABLE toRationalDecimal #-}

-- | Convert from `Rational` to a `Decimal` backed by `Integer`. `PrecisionLoss` will be
-- thrown if conversion cannot be achieved without any loss of data. In case that rounding
-- is acceptable use `fromRationalDecimalRounded`
--
-- @since 0.2.0
fromRationalDecimal ::
     forall m r s. (MonadThrow m, KnownNat s)
  => Rational
  -> m (Decimal r s Integer)
fromRationalDecimal rational
  | denominator rational == 0 = throwM DivideByZero
  | fromIntegral t /= scaledRat = throwM (PrecisionLoss rational s)
  | otherwise = pure truncated
  where
    truncated@(Decimal t) = Decimal (truncate scaledRat) :: Decimal r s Integer
    scaledRat = rational * (d % 1)
    s = natVal (Proxy :: Proxy s)
    d = 10 ^ s
{-# INLINABLE fromRationalDecimal #-}

-- |
--
-- @since 0.2.0
fromRationalDecimalBounded ::
     (MonadThrow m, KnownNat s, Integral p, Bounded p)
  => Rational
  -> m (Decimal r s p)
fromRationalDecimalBounded r = fromRationalDecimal r >>= fromIntegerDecimalBounded
{-# INLINABLE fromRationalDecimalBounded #-}

fromRationalDecimalRounded ::
     forall m r s p. (MonadThrow m, KnownNat s, Round r Integer, Bounded p, Integral p)
  => Rational
  -> m (Decimal r s p)
fromRationalDecimalRounded rational
  | denominator rational == 0 = throwM DivideByZero
  | otherwise =
    fromIntegerDecimalBounded $
    roundDecimal (Decimal (truncate scaledRat) :: Decimal r (s + 1) Integer)
  where
    scaledRat = rational * (d % 1)
    d = 10 ^ (natVal (Proxy :: Proxy s) + 1)
{-# INLINABLE fromRationalDecimalRounded #-}


-- | Compute signum of a decimal, always one of 1, 0 or -1
signumDecimal :: KnownNat s => Decimal r s Integer -> Decimal r s Integer
signumDecimal (Decimal d) = fromIntegerDecimal (signum d)
{-# INLINABLE signumDecimal #-}

-- | Compute signum of a decimal, always one of 1, 0 or -1
signumDecimalBounded ::
     (KnownNat s, MonadThrow m, Integral p, Bounded p)
  => Decimal r s p
  -> m (Decimal r s p)
signumDecimalBounded d = fromIntegerDecimalBounded $ signumDecimal (toInteger <$> d)
{-# INLINABLE signumDecimalBounded #-}


-----------------------------------
-- Showing ------------------------
-----------------------------------

instance (Integral p, KnownNat s) => Show (Decimal r s p) where
  show d@(Decimal a)
    | s == 0 = show $ toInteger a
    | r == 0 = printf ("%d." ++ replicate s '0') q
    | signum r < 0 && q == 0 = "-" ++ formatted
    | otherwise = formatted
    where
      formatted = printf fmt q (abs r)
      s = fromInteger $ getScale d
      fmt = "%d.%0" ++ show s ++ "u"
      (q, r) = quotRem (toInteger a) (10 ^ s)

-----------------------------------
-- Parsing ------------------------
-----------------------------------

maxBoundCharsCount :: forall a . (Integral a, Bounded a) => Proxy a -> Int
maxBoundCharsCount _ = length (show (toInteger (maxBound :: a)))

minBoundCharsCount :: forall a . (Integral a, Bounded a) => Proxy a -> Int
minBoundCharsCount _ = length (show (toInteger (minBound :: a)))

fromIntegersScaleBounded ::
     forall m a s. (MonadThrow m, Integral a, Bounded a, KnownNat s)
  => Proxy s
  -> Integer
  -> Integer
  -> m a
fromIntegersScaleBounded ps x y
  | xs > toInteger (maxBound :: a) = throwM Overflow
  | xs < toInteger (minBound :: a) = throwM Underflow
  | otherwise = pure $ fromInteger xs
  where s = natVal ps
        xs = x * (10 ^ s) + y
{-# INLINABLE fromIntegersScaleBounded #-}


parseDecimalBounded ::
     forall r s p. (KnownNat s, Bounded p, Integral p)
  => Bool
  -> String
  -> Either String (Decimal r s p)
parseDecimalBounded checkForPlusSign rawInput
  | not (null tooMuch) = Left "Input is too big for parsing as a bounded Decimal value"
  | otherwise = do
    (sign, signLeftOver) <- getSign input
    -- by now we conditionally extracted the sign (+/-)
    (num, leftOver) <- digits signLeftOver
    let s = fromIntegral (natVal spx) :: Int
    case uncons leftOver of
      Nothing -> toStringError (fromIntegersScaleBounded spx (sign * num) 0)
      Just ('.', digitsTxt)
        | length digitsTxt > s -> Left $ "Too much text after the decimal: " ++ digitsTxt
      Just ('.', digitsTxt)
        | not (null digitsTxt) -> do
          (decimalDigits, extraTxt) <- digits (digitsTxt ++ replicate (s - length digitsTxt) '0')
          unless (null extraTxt) $ Left $ "Unrecognized digits: " ++ digitsTxt
          toStringError (fromIntegersScaleBounded spx (sign * num) (sign * decimalDigits))
      _ -> Left $ "Unrecognized left over text: " ++ leftOver
  where
    spx = Proxy :: Proxy s
    toStringError =
      \case
        Left exc
          | Just Underflow <- fromException exc ->
            Left $ "Number is too small to be represented as decimal: " ++ input
        Left exc
          | Just Overflow <- fromException exc ->
            Left $ "Number is too big to be represented as decimal: " ++ input
        Left err -> Left $ "Unexpected error: " ++ displayException err
        Right val -> Right (Decimal val)
    maxChars =
      2 + max (maxBoundCharsCount (Proxy :: Proxy p)) (minBoundCharsCount (Proxy :: Proxy p))
    {-- ^ account for possible dot in the decimal and an extra preceding 0 -}
    (input, tooMuch) = splitAt maxChars rawInput
    getSign str =
      if (minBound :: p) >= 0
        then Right (1, str)
        else case uncons str of
               Nothing -> Left "Input String is empty"
               Just ('-', strLeftOver) -> Right (-1, strLeftOver)
               Just ('+', strLeftOver)
                 | checkForPlusSign -> Right (1, strLeftOver)
               _ -> Right (1, str)

digits :: Num a => String -> Either String (a, String)
digits str
  | null h = Left "Input does not start with a digit"
  | otherwise = Right (F.foldl' go 0 h, t)
  where
    (h, t) = span isDigit str
    go n d = n * 10 + fromIntegral (digitToInt d)

