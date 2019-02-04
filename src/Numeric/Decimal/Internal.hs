{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
module Numeric.Decimal.Internal
  ( Decimal(..)
  , Round(..)
  , wrapDecimal
  , unwrapDecimal
  , splitDecimal
  , getScale
  , fromNum
  , parseDecimalBounded
  -- * Algebra
  , plusDecimal
  , minusDecimal
  , timesDecimal
  , signumDecimal
  , timesDecimalBounded
  , timesDecimalRounded
  , divideDecimal
  , quotRemBounded
  , quotRemDecimalBounded
  , fromIntegerDecimalBounded
  , fromRationalDecimalRounded
  , liftDecimal
  , liftDecimal2
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
  ) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch
import           Data.Char
import           Data.Foldable       as F
import           Data.Int
import           Data.List
import           Data.Proxy
import           Data.Ratio
import           Data.Word
import           GHC.Generics        (Generic)
import           GHC.TypeLits
import           Text.Printf


-- | Decimal number with custom precision (@p@) and type level scaling (@s@) parameter (i.e. number
-- of digits after the decimal point). As well as the rounding (@r@) strategy to use
newtype Decimal r (s :: Nat) p = Decimal p
  deriving (Enum, Ord, Eq, NFData, Functor, Generic)

instance Applicative (Decimal r s) where
  pure = Decimal
  {-# INLINABLE pure #-}
  (<*>) (Decimal f) (Decimal x) = Decimal (f x)
  {-# INLINABLE (<*>) #-}


class Round r where
  roundDecimal :: (Integral p, KnownNat k) => Decimal r (n + k) p -> Decimal r n p


-- | Get the scale of the `Decimal`. Argument is not evaluated.
getScale :: forall r s p . KnownNat s => Decimal r s p -> Int
getScale _ = fromIntegral (natVal (Proxy :: Proxy s))

-- | Split the number at the decimal point, i.e. whole number and the fraction
splitDecimal :: (Integral p, KnownNat s) => Decimal r s p -> (p, p)
splitDecimal d@(Decimal v) = v `quotRem` (10 ^ getScale d)

-- | Wrap an `Integral` as a `Decimal`. No scaling will be done.
wrapDecimal :: Integral p => p -> Decimal r s p
wrapDecimal = Decimal

-- | Get out the underlying representation for the decimal number. No scaling will be done.
unwrapDecimal :: Decimal r s p -> p
unwrapDecimal (Decimal p) = p

-- | This operation is susceptible to overflows, since it performs the scaling.
fromNum :: forall r s p . (Num p, KnownNat s) => p -> Decimal r s p
fromNum x = Decimal (x * (10 ^ s))
  where
    s = natVal (Proxy :: Proxy s)
{-# INLINABLE fromNum #-}


liftDecimal :: (p1 -> p2) -> Decimal r s p1 -> Decimal r s p2
liftDecimal f (Decimal x) = Decimal (f x)
{-# INLINABLE liftDecimal #-}

liftDecimal2 :: (p1 -> p2 -> p3) -> Decimal r s p1 -> Decimal r s p2 -> Decimal r s p3
liftDecimal2 f (Decimal x) (Decimal y) = Decimal (f x y)
{-# INLINABLE liftDecimal2 #-}

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

instance (Round r, KnownNat s) => Num (Decimal r s Integer) where
  (+) = liftA2 (+)
  {-# INLINABLE (+) #-}
  (-) = liftDecimal2 (-)
  {-# INLINABLE (-) #-}
  (*) = liftDecimal2 (*)
  {-# INLINABLE (*) #-}
  signum = signumDecimal
  {-# INLINABLE signum #-}
  abs = fmap abs
  {-# INLINABLE abs #-}
  fromInteger = fromNum
  {-# INLINABLE fromInteger #-}

instance (Round r, KnownNat s) => Real (Decimal r s Integer) where
  toRational (Decimal p) = p % (10 ^ natVal (Proxy :: Proxy s))
  {-# INLINABLE toRational #-}

-- | The order of fractional and negation for literals prevents rational numbers to be negative in
-- `fromRational` function, which can cause some issues in rounding:
--
-- >>> fromRational (-23.5) :: Either SomeException (Decimal RoundHalfUp 0 Integer)
-- Right -23
-- >>> -23.5 :: Either SomeException (Decimal RoundHalfUp 0 Integer)
-- Right -24
instance (MonadThrow m, Round r, KnownNat s) => Fractional (m (Decimal r s Integer)) where
  (/) = bindM2 divideDecimal
  {-# INLINABLE (/) #-}
  fromRational = fromRationalDecimalRounded
  {-# INLINABLE fromRational #-}

instance (MonadThrow m, Round r, KnownNat s) => Num (m (Decimal r s Integer)) where
  (+) = liftA2 (+)
  {-# INLINABLE (+) #-}
  (-) = liftA2 (-)
  {-# INLINABLE (-) #-}
  (*) x y = roundDecimal <$> liftA2 timesDecimal x y
  {-# INLINABLE (*) #-}
  signum = fmap signumDecimal
  {-# INLINABLE signum #-}
  abs = fmap (fmap abs)
  {-# INLINABLE abs #-}
  fromInteger = pure . fromNum
  {-# INLINABLE fromInteger #-}


-----------------------------------
-- Bounded Integral instances -----
-----------------------------------


instance (MonadThrow m, Round r, KnownNat s) => Num (m (Decimal r s Int)) where
  (+) = bindM2 plusDecimal
  {-# INLINABLE (+) #-}
  (-) = bindM2 minusDecimal
  {-# INLINABLE (-) #-}
  (*) = bindM2 timesDecimalRounded
  {-# INLINABLE (*) #-}
  signum = fmap signumDecimal
  {-# INLINABLE signum #-}
  abs = fmap (fmap abs)
  {-# INLINABLE abs #-}
  fromInteger = fmap Decimal . fromIntegerScaleBounded (Proxy :: Proxy s)
  {-# INLINABLE fromInteger #-}

instance (MonadThrow m, Round r, KnownNat s) => Num (m (Decimal r s Int8)) where
  (+) = bindM2 plusDecimal
  {-# INLINABLE (+) #-}
  (-) = bindM2 minusDecimal
  {-# INLINABLE (-) #-}
  (*) = bindM2 timesDecimalRounded
  {-# INLINABLE (*) #-}
  signum = fmap signumDecimal
  {-# INLINABLE signum #-}
  abs = fmap (fmap abs)
  {-# INLINABLE abs #-}
  fromInteger = fmap Decimal . fromIntegerScaleBounded (Proxy :: Proxy s)
  {-# INLINABLE fromInteger #-}

instance (MonadThrow m, Round r, KnownNat s) => Num (m (Decimal r s Int16)) where
  (+) = bindM2 plusDecimal
  {-# INLINABLE (+) #-}
  (-) = bindM2 minusDecimal
  {-# INLINABLE (-) #-}
  (*) = bindM2 timesDecimalRounded
  {-# INLINABLE (*) #-}
  signum = fmap signumDecimal
  {-# INLINABLE signum #-}
  abs = fmap (fmap abs)
  {-# INLINABLE abs #-}
  fromInteger = fmap Decimal . fromIntegerScaleBounded (Proxy :: Proxy s)
  {-# INLINABLE fromInteger #-}

instance (MonadThrow m, Round r, KnownNat s) => Num (m (Decimal r s Int32)) where
  (+) = bindM2 plusDecimal
  {-# INLINABLE (+) #-}
  (-) = bindM2 minusDecimal
  {-# INLINABLE (-) #-}
  (*) = bindM2 timesDecimalRounded
  {-# INLINABLE (*) #-}
  signum = fmap signumDecimal
  {-# INLINABLE signum #-}
  abs = fmap (fmap abs)
  {-# INLINABLE abs #-}
  fromInteger = fmap Decimal . fromIntegerScaleBounded (Proxy :: Proxy s)
  {-# INLINABLE fromInteger #-}

instance (MonadThrow m, Round r, KnownNat s) => Num (m (Decimal r s Int64)) where
  (+) = bindM2 plusDecimal
  {-# INLINABLE (+) #-}
  (-) = bindM2 minusDecimal
  {-# INLINABLE (-) #-}
  (*) = bindM2 timesDecimalRounded
  {-# INLINABLE (*) #-}
  signum = fmap signumDecimal
  {-# INLINABLE signum #-}
  abs = fmap (fmap abs)
  {-# INLINABLE abs #-}
  fromInteger = fmap Decimal . fromIntegerScaleBounded (Proxy :: Proxy s)
  {-# INLINABLE fromInteger #-}

instance (MonadThrow m, Round r, KnownNat s) => Num (m (Decimal r s Word)) where
  (+) = bindM2 plusDecimal
  {-# INLINABLE (+) #-}
  (-) = bindM2 minusDecimal
  {-# INLINABLE (-) #-}
  (*) = bindM2 timesDecimalRounded
  {-# INLINABLE (*) #-}
  signum = fmap signumDecimal
  {-# INLINABLE signum #-}
  abs = id
  {-# INLINABLE abs #-}
  fromInteger = fmap Decimal . fromIntegerScaleBounded (Proxy :: Proxy s)
  {-# INLINABLE fromInteger #-}

instance (MonadThrow m, Round r, KnownNat s) => Num (m (Decimal r s Word8)) where
  (+) = bindM2 plusDecimal
  {-# INLINABLE (+) #-}
  (-) = bindM2 minusDecimal
  {-# INLINABLE (-) #-}
  (*) = bindM2 timesDecimalRounded
  {-# INLINABLE (*) #-}
  signum = fmap signumDecimal
  {-# INLINABLE signum #-}
  abs = id
  {-# INLINABLE abs #-}
  fromInteger = fmap Decimal . fromIntegerScaleBounded (Proxy :: Proxy s)
  {-# INLINABLE fromInteger #-}

instance (MonadThrow m, Round r, KnownNat s) => Num (m (Decimal r s Word16)) where
  (+) = bindM2 plusDecimal
  {-# INLINABLE (+) #-}
  (-) = bindM2 minusDecimal
  {-# INLINABLE (-) #-}
  (*) = bindM2 timesDecimalRounded
  {-# INLINABLE (*) #-}
  signum = fmap signumDecimal
  {-# INLINABLE signum #-}
  abs = id
  {-# INLINABLE abs #-}
  fromInteger = fmap Decimal . fromIntegerScaleBounded (Proxy :: Proxy s)
  {-# INLINABLE fromInteger #-}

instance (MonadThrow m, Round r, KnownNat s) => Num (m (Decimal r s Word32)) where
  (+) = bindM2 plusDecimal
  {-# INLINABLE (+) #-}
  (-) = bindM2 minusDecimal
  {-# INLINABLE (-) #-}
  (*) = bindM2 timesDecimalRounded
  {-# INLINABLE (*) #-}
  signum = fmap signumDecimal
  {-# INLINABLE signum #-}
  abs = id
  {-# INLINABLE abs #-}
  fromInteger = fmap Decimal . fromIntegerScaleBounded (Proxy :: Proxy s)
  {-# INLINABLE fromInteger #-}

instance (MonadThrow m, Round r, KnownNat s) => Num (m (Decimal r s Word64)) where
  (+) = bindM2 plusDecimal
  {-# INLINABLE (+) #-}
  (-) = bindM2 minusDecimal
  {-# INLINABLE (-) #-}
  (*) = bindM2 timesDecimalRounded
  {-# INLINABLE (*) #-}
  signum = fmap signumDecimal
  {-# INLINABLE signum #-}
  abs = id
  {-# INLINABLE abs #-}
  fromInteger = fmap Decimal . fromIntegerScaleBounded (Proxy :: Proxy s)
  {-# INLINABLE fromInteger #-}

instance (MonadThrow m, Round r, KnownNat s) => Fractional (m (Decimal r s Int)) where
  (/) = bindM2 divideDecimal
  {-# INLINABLE (/) #-}
  fromRational r = fromRational r >>= fromIntegerDecimalBounded
  {-# INLINABLE fromRational #-}

instance (MonadThrow m, Round r, KnownNat s) => Fractional (m (Decimal r s Int8)) where
  (/) = bindM2 divideDecimal
  {-# INLINABLE (/) #-}
  fromRational r = fromRational r >>= fromIntegerDecimalBounded
  {-# INLINABLE fromRational #-}

instance (MonadThrow m, Round r, KnownNat s) => Fractional (m (Decimal r s Int16)) where
  (/) = bindM2 divideDecimal
  {-# INLINABLE (/) #-}
  fromRational r = fromRational r >>= fromIntegerDecimalBounded
  {-# INLINABLE fromRational #-}

instance (MonadThrow m, Round r, KnownNat s) => Fractional (m (Decimal r s Int32)) where
  (/) = bindM2 divideDecimal
  {-# INLINABLE (/) #-}
  fromRational r = fromRational r >>= fromIntegerDecimalBounded
  {-# INLINABLE fromRational #-}


instance (MonadThrow m, Round r, KnownNat s) => Fractional (m (Decimal r s Int64)) where
  (/) = bindM2 divideDecimal
  {-# INLINABLE (/) #-}
  fromRational r = fromRational r >>= fromIntegerDecimalBounded
  {-# INLINABLE fromRational #-}

instance (MonadThrow m, Round r, KnownNat s) => Fractional (m (Decimal r s Word)) where
  (/) = bindM2 divideDecimal
  {-# INLINABLE (/) #-}
  fromRational r = fromRational r >>= fromIntegerDecimalBounded
  {-# INLINABLE fromRational #-}

instance (MonadThrow m, Round r, KnownNat s) => Fractional (m (Decimal r s Word8)) where
  (/) = bindM2 divideDecimal
  {-# INLINABLE (/) #-}
  fromRational r = fromRational r >>= fromIntegerDecimalBounded
  {-# INLINABLE fromRational #-}

instance (MonadThrow m, Round r, KnownNat s) => Fractional (m (Decimal r s Word16)) where
  (/) = bindM2 divideDecimal
  {-# INLINABLE (/) #-}
  fromRational r = fromRational r >>= fromIntegerDecimalBounded
  {-# INLINABLE fromRational #-}

instance (MonadThrow m, Round r, KnownNat s) => Fractional (m (Decimal r s Word32)) where
  (/) = bindM2 divideDecimal
  {-# INLINABLE (/) #-}
  fromRational r = fromRational r >>= fromIntegerDecimalBounded
  {-# INLINABLE fromRational #-}

instance (MonadThrow m, Round r, KnownNat s) => Fractional (m (Decimal r s Word64)) where
  (/) = bindM2 divideDecimal
  {-# INLINABLE (/) #-}
  fromRational r = fromRational r >>= fromIntegerDecimalBounded
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
  | (sameSig && sigX ==  1 && x > maxBound - y) = throwM Overflow
  | (sameSig && sigX == -1 && x < minBound - y) = throwM Underflow
  | otherwise = pure (x + y)
  where
    sigX = signum x
    sigY = signum y
    sameSig = sigX == sigY
{-# INLINABLE plusBounded #-}

-- | Subtract two bounded numbers while checking for `Overflow`/`Underflow`
minusBounded :: (MonadThrow m, Eq a, Ord a, Num a, Bounded a) => a -> a -> m a
minusBounded x y
  | (sigY == -1 && x > maxBound + y) = throwM Overflow
  | (sigY ==  1 && x < minBound + y) = throwM Underflow
  | otherwise = pure (x - y)
  where sigY = signum y
{-# INLINABLE minusBounded #-}

-- | Divide two decimal numbers while checking for `Overflow` and `DivideByZero`
divBounded :: (MonadThrow m, Integral a, Bounded a) => a -> a -> m a
divBounded x y
  | y == 0 = throwM DivideByZero
  | sigY == -1 && y == -1 && x == minBound = throwM Overflow
    ------------------- ^ Here we deal with special case overflow when (minBound * (-1))
  | otherwise = pure (x `div` y)
  where
    sigY = signum y
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
  | (sigY == -1 && y == -1 && x == minBound) = throwM Overflow
  | (signum x == -1 && x == -1 && y == minBound) = throwM Overflow
  | (sigY ==  1 && (minBoundQuotY > x || x > maxBoundQuotY)) = eitherOverUnder
  | (sigY == -1 && y /= -1 && (minBoundQuotY < x || x < maxBoundQuotY)) = eitherOverUnder
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


-- | Multiply two decimal numbers, while rounding the result according to the rounding strategy.
timesDecimalRounded ::
     (MonadThrow m, KnownNat s, Round r, Integral p, Bounded p)
  => Decimal r s p
  -> Decimal r s p
  -> m (Decimal r s p)
timesDecimalRounded dx dy =
  fromIntegerDecimalBounded $ roundDecimal $ timesDecimal (fmap toInteger dx) (fmap toInteger dy)
{-# INLINABLE timesDecimalRounded #-}

fromRationalDecimalRounded ::
     forall m r s p. (MonadThrow m, KnownNat s, Round r, Integral p)
  => Rational
  -> m (Decimal r s p)
fromRationalDecimalRounded rational
  | denominator rational == 0 = throwM DivideByZero
  | otherwise = pure $ roundDecimal (Decimal (truncate scaledRat) :: Decimal r (s + 1) p)
  where
    scaledRat = rational * (d % 1)
    d = 10 ^ (natVal (Proxy :: Proxy s) + 1)
{-# INLINABLE fromRationalDecimalRounded #-}


-- | Compute signum of a decimal, always one of 1, 0 or -1
signumDecimal :: (Num p, KnownNat s) => Decimal r s p -> Decimal r s p
signumDecimal (Decimal d) = fromNum (signum d) -- It is safe to scale since signum does not widen
                                               -- the range, thus will always fall into a valid
                                               -- value
{-# INLINABLE signumDecimal #-}


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
      s = getScale d
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
      Nothing -> do
        toStringError (fromIntegersScaleBounded spx (sign * num) 0)
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
    go n d = (n * 10 + fromIntegral (digitToInt d))

