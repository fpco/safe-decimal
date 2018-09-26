{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
module Numeric.Decimal.Internal
  ( Decimal(..)
  , Round(..)
  , wrapDecimal
  , unwrapDecimal
  , splitDecimal
  , getScale
  -- * Conversion
  , toScientific
  , fromScientific
  , fromScientificBounded
  , fromNum
  -- * Helpers
  , timesDecimal
  , liftDecimal
  , liftDecimal2
  , bind2DecimalM
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
import           Data.Proxy
import           Data.Scientific
import           GHC.TypeLits
import           Text.Printf

-- | Decimal number with custom precision (@p@) and type level scaling (@s@) parameter (i.e. number
-- of digits after the decimal point). As well as the rounding (@r@) strategy to use
newtype Decimal r (s :: Nat) p = Decimal p
  deriving (Enum, Ord, Eq, NFData, Functor)

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

bind2DecimalM ::
     Monad m
  => (p1 -> p2 -> m p)
  -> m (Decimal r1 s1 p1)
  -> m (Decimal r2 s2 p2)
  -> m (Decimal r s p)
bind2DecimalM f dx dy = do
  Decimal x <- dx
  Decimal y <- dy
  Decimal <$> f x y
{-# INLINABLE bind2DecimalM #-}


instance Bounded p => Bounded (Decimal r s p) where
  minBound = Decimal minBound
  maxBound = Decimal maxBound

instance (Round r, KnownNat s) => Num (Decimal r s Integer) where
  (+) = liftA2 (+)
  {-# INLINABLE (+) #-}
  (-) = liftDecimal2 (-)
  {-# INLINABLE (-) #-}
  (*) = liftDecimal2 (*)
  {-# INLINABLE (*) #-}
  signum = fmap signum
  {-# INLINABLE signum #-}
  abs = fmap abs
  {-# INLINABLE abs #-}
  fromInteger = fromNum
  {-# INLINABLE fromInteger #-}

instance (Round r, Integral p, Bounded p, KnownNat s) =>
         Num (Either ArithException (Decimal r s p)) where
  (+) = bind2DecimalM plusBounded
  {-# INLINABLE (+) #-}
  (-) = bind2DecimalM minusBounded
  {-# INLINABLE (-) #-}
  (*) adx ady = do
    dx <- adx
    dy <- ady
    roundDecimal <$> timesDecimal dx dy
  {-# INLINABLE (*) #-}
  signum = fmap (liftDecimal signum)
  {-# INLINABLE signum #-}
  abs = fmap (liftDecimal abs)
  {-# INLINABLE abs #-}
  fromInteger = fmap Decimal . fromIntegerScaleBounded (Proxy :: Proxy s)
  {-# INLINABLE fromInteger #-}


-- | Add two bounded numbers while checking for `Overflow`/`Underflow`
plusBounded :: (Integral a, Bounded a) => a -> a -> Either ArithException a
plusBounded x y
  | (sameSig && sigX ==  1 && x > maxBound - y) = Left Overflow
  | (sameSig && sigX == -1 && x < minBound - y) = Left Underflow
  | otherwise = Right (x + y)
  where
    sigX = signum x
    sigY = signum y
    sameSig = sigX == sigY
{-# INLINABLE plusBounded #-}

-- | Subtract two bounded numbers while checking for `Overflow`/`Underflow`
minusBounded :: (Integral a, Bounded a) => a -> a -> Either ArithException a
minusBounded x y
  | (sigY == -1 && x > maxBound + y) = Left Overflow
  | (sigY ==  1 && x < minBound + y) = Left Underflow
  | otherwise = Right (x - y)
  where sigY = signum y
{-# INLINABLE minusBounded #-}

-- | Add two decimal numbers while checking for `Overflow`
divBounded :: (Integral a, Bounded a) => a -> a -> Either ArithException a
divBounded x y
  | y == 0 = Left DivideByZero
  | sigY == -1 && y == -1 && x == minBound = Left Overflow
    -------------------- ^ Here we deal with special case overflow when (minBound * (-1))
  | otherwise = Right (x `div` y)
  where
    sigY = signum y


-- | Add two decimal numbers while checking for `Overflow`
quotBounded :: (Integral a, Bounded a) => a -> a -> Either ArithException a
quotBounded x y
  | y == 0 = Left DivideByZero
  | sigY == -1 && y == -1 && x == minBound = Left Overflow
    -------------------- ^ Here we deal with special case overflow when (minBound * (-1))
  | otherwise = Right (x `quot` y)
  where
    sigY = signum y


-- | Add two decimal numbers while checking for `Overflow`
timesBounded :: (Integral a, Bounded a) => a -> a -> Either ArithException a
timesBounded x y
  | (sigY == -1 && y == -1 && x == minBound) = Left Overflow
  | (signum x == -1 && x == -1 && y == minBound) = Left Overflow
  | (sigY ==  1 && (minBoundQuotY > x || x > maxBoundQuotY)) = Left Underflow
  | (sigY == -1 && y /= -1 && (minBoundQuotY < x || x < maxBoundQuotY)) = Left Overflow
  | otherwise = Right (x * y)
  where
    sigY = signum y
    maxBoundQuotY = maxBound `quot` y
    minBoundQuotY = minBound `quot` y
{-# INLINABLE timesBounded #-}


fromIntegerBounded ::
     forall a. (Integral a, Bounded a)
  => Integer
  -> Either ArithException a
fromIntegerBounded x
  | x > toInteger (maxBound :: a) = Left Overflow
  | x < toInteger (minBound :: a) = Left Underflow
  | otherwise = Right $ fromInteger x
{-# INLINABLE fromIntegerBounded #-}

fromIntegerScaleBounded ::
     forall a s. (Integral a, Bounded a, KnownNat s)
  => Proxy s
  -> Integer
  -> Either ArithException a
fromIntegerScaleBounded ps x
  | xs > toInteger (maxBound :: a) = Left Overflow
  | xs < toInteger (minBound :: a) = Left Underflow
  | otherwise = Right $ fromInteger xs
  where s = natVal ps
        xs = x * (10 ^ s)
{-# INLINABLE fromIntegerScaleBounded #-}


-- | Multiply two decimal numbers, adjusting their scale at the type level as well.
timesDecimal ::
     (Integral p, Bounded p)
  => Decimal r s1 p
  -> Decimal r s2 p
  -> Either ArithException (Decimal r (s1 + s2) p)
timesDecimal (Decimal x) (Decimal y) = Decimal <$> timesBounded x y
{-# INLINABLE timesDecimal #-}



instance (Integral p, KnownNat s) => Show (Decimal r s p) where
  show d@(Decimal a)
    | s == 0 = show $ toInteger a
    | r == 0 = printf ("%u." ++ replicate s '0') $ toInteger q
    | otherwise = printf fmt (toInteger q) (toInteger r)
    where s = getScale d
          fmt = "%u.%0" ++ show s ++ "u"
          (q, r) = quotRem a (10 ^ s)


---- Scientific

-- | Convert Decimal to Scientific
toScientific :: (Integral p, KnownNat s) => Decimal r s p -> Scientific
toScientific dec = scientific (toInteger (unwrapDecimal dec)) (negate (getScale dec))

-- | Convert Scientific to Decimal without loss of precision. Will return `Left` `Underflow` if
-- `Scientific` has too many decimal places, more than `Decimal` scaling is capable to handle.
fromScientific :: forall r s . KnownNat s => Scientific -> Either ArithException (Decimal r s Integer)
fromScientific num
  | point10 > s = Left Underflow
  | otherwise = Right (Decimal (coefficient num * 10 ^ (s - point10)))
  where
      s = natVal (Proxy :: Proxy s)
      point10 = toInteger (negate (base10Exponent num))

-- | Convert from Scientific to Decimal while checking for Overflow/Underflow 
fromScientificBounded ::
     forall r s p. (Integral p, Bounded p, KnownNat s)
  => Scientific
  -> Either ArithException (Decimal r s p)
fromScientificBounded num = do
  Decimal integer :: Decimal r s Integer <- fromScientific num
  Decimal <$> fromIntegerBounded integer


-- TODO: Convert with Rounding
-- fromScientificRounded ::
--      forall r s. (Round r, KnownNat s)
--   => Scientific
--   -> ArithException (Decimal r s Integer)



-- instance KnownNat s => ToJSON (Decimal s) where
--   toJSON (Decimal c) =
--     Number (scientific (toInteger c) (negate (fromIntegral (natVal (Proxy :: Proxy s)))))

-- instance KnownNat s => FromJSON (Decimal s) where
--   parseJSON =
--     withScientific "Extected number" $ \num ->
--       let exp10 = negate (base10Exponent num)
--           s = fromIntegral (natVal (Proxy :: Proxy s))
--        in if exp10 > s
--             then fail $ "Too many digits after the decimal: " ++ show num
--             else do
--               let n = coefficient (num * 10 ^ (s - exp10))
--               guard (n >= 0 && n <= fromIntegral (maxBound :: Int64))
--               return $ Decimal (fromInteger n)


-- Parsing


-- parseDecimal :: forall s. KnownNat s => T.Text -> Either String (Decimal s)
-- parseDecimal input
--   | T.length input > 20 = Left "Input is too big for parsing as a Decimal value"
--   | otherwise = do
--     (num, leftOver) <- T.decimal input
--     let s = fromIntegral (natVal (Proxy :: Proxy s)) :: Int
--     case T.uncons leftOver of
--       Nothing -> do
--         return ((num :: Int) $. (0 :: Int))
--       Just ('.', digitsTxt)
--         | T.length digitsTxt > s -> Left $ "Too much text after the decimal: " ++ T.unpack digitsTxt
--       Just ('.', digitsTxt)
--         | not (T.null digitsTxt) -> do
--           (decimalDigits, extraTxt) <-
--             T.decimal (digitsTxt <> T.replicate (s - T.length digitsTxt) "0")
--           unless (T.null extraTxt) $ Left $ "Unrecognized digits: " ++ T.unpack digitsTxt
--             -- TODO: when switching to safe-decimal ($.) will return Either
--           return ((num :: Int) $. (decimalDigits :: Int))
--       _ -> Left $ "Unrecognized left over text: " ++ T.unpack leftOver
