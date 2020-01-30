{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.Decimal.BoundedArithmetic
  ( -- ** Bounded
    plusBounded
  , minusBounded
  , timesBounded
  , absBounded
  , fromIntegerBounded
  , divBounded
  , quotBounded
  , quotRemBounded
  ) where

import Control.Exception
import Control.Monad.Catch

-----------------------------------
-- Bounded arithmetics ------------
-----------------------------------

-- | Add two bounded numbers while checking for `Overflow`/`Underflow`
--
-- @since 0.1.0
plusBounded :: (MonadThrow m, Ord a, Num a, Bounded a) => a -> a -> m a
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
--
-- @since 0.1.0
minusBounded :: (MonadThrow m, Ord a, Num a, Bounded a) => a -> a -> m a
minusBounded x y
  | sigY == -1 && x > maxBound + y = throwM Overflow
  | sigY ==  1 && x < minBound + y = throwM Underflow
  | otherwise = pure (x - y)
  where sigY = signum y
{-# INLINABLE minusBounded #-}

-- | Compute absolute value, while checking for `Overflow`
--
-- @since 0.2.0
absBounded :: (MonadThrow m, Num p, Ord p) => p -> m p
absBounded d
  | absd < 0 = throwM Overflow
  | otherwise = pure absd
  where
    absd = abs d
{-# INLINABLE absBounded #-}


-- | Divide two numbers while checking for `Overflow` and `DivideByZero`
--
-- @since 0.1.0
divBounded :: (MonadThrow m, Integral a, Bounded a) => a -> a -> m a
divBounded x y
  | y == 0 = throwM DivideByZero
  | signum y == -1 && y == -1 && x == minBound = throwM Overflow
    ------------------- ^ Here we deal with special case overflow when (minBound * (-1))
  | otherwise = pure (x `div` y)
{-# INLINABLE divBounded #-}


-- | Find quotient of two numbers while checking for `Overflow` and `DivideByZero`
--
-- @since 0.1.0
quotBounded :: (MonadThrow m, Integral a, Bounded a) => a -> a -> m a
quotBounded x y
  | y == 0 = throwM DivideByZero
  | sigY == -1 && y == -1 && x == minBound = throwM Overflow
    ------------------- ^ Here we deal with special case overflow when (minBound * (-1))
  | otherwise = pure (x `quot` y)
  where
    sigY = signum y -- Guard against wraparound in case of unsigned Word
{-# INLINABLE quotBounded #-}

-- | Find quotient an remainder of two numbers while checking for `Overflow` and
-- `DivideByZero`
--
-- @since 0.1.0
quotRemBounded :: (MonadThrow m, Integral a, Bounded a) => a -> a -> m (a, a)
quotRemBounded x y
  | y == 0 = throwM DivideByZero
  | sigY == -1 && y == -1 && x == minBound = throwM Overflow
  | otherwise = pure (x `quotRem` y)
  where
    sigY = signum y
{-# INLINABLE quotRemBounded #-}


-- | Multiply two numbers while checking for `Overflow`
--
-- @since 0.1.0
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

-- | Convert from an unbounded `Integer` to a `Bounded` `Integral`, while checking for
-- bounds and raising `Overflow`/`Underflow`
--
-- @since 0.1.0
fromIntegerBounded ::
     forall m a. (MonadThrow m, Integral a, Bounded a)
  => Integer
  -> m a
fromIntegerBounded x
  | x > toInteger (maxBound :: a) = throwM Overflow
  | x < toInteger (minBound :: a) = throwM Underflow
  | otherwise = pure $ fromInteger x
{-# INLINABLE fromIntegerBounded #-}

