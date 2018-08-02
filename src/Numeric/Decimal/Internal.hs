{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Numeric.Decimal.Internal
  ( Decimal
  , RoundHalfUp
  , toDecimal
  , unDecimal
  , splitDecimal
  -- , decimal
  , getScale
  -- -- * Conversion
  -- , fromScientific
  -- , toScientific
  -- -- * Operations
  -- , (.$)
  -- , (+$)
  -- , (-$)
  -- , (*$)
  , sumDecimal
  -- , quotRemDecimal
  -- , splitDecimal
  -- -- * Rounding
  -- , roundHalfUp
  ) where

import           Control.DeepSeq
import           Control.Monad
import           Data.Proxy
import           GHC.TypeLits
import           Text.Printf

import           Numeric.ArithM

-- | Decimal number with custom precision and type level scaling parameter (i.e. number of digits
-- after the decimal point).
newtype Decimal r (s :: Nat) p = Decimal { unDecimal :: p }
  deriving (Ord, Eq, NFData, Functor)


data RoundHalfUp


-- | Get the scale of the `Decimal`. Argument is not evaluated.
getScale :: forall r s p . KnownNat s => Decimal r s p -> Int
getScale _ = fromIntegral (natVal (Proxy :: Proxy s))

-- | Split the number at the decimal point, i.e. whole number and the fraction
splitDecimal :: forall r s p . (Integral p, KnownNat s) => Decimal r s p -> (p, p)
splitDecimal d@(Decimal v) = v `quotRem` (10 ^ getScale d)

-- | Construct a `Decimal` from an `Integral`.
toDecimal :: p -> Decimal r s p
toDecimal = Decimal


-- -- | Maximum supported value for the decimal
-- maxDecimal :: Int64
-- maxDecimal = maxBound

-- -- | Construct a decimal number from the whole number and the fractional part.  Be careful with
-- -- trailing zeros in factional part, since depending on the scale of decimal it will mean different
-- -- things, eg. ...
-- --
-- -- >>> decimal 5 60 :: Either DecimalError (Decimal 2)
-- -- Right 5.60
-- -- >>> decimal 5 60 :: Either DecimalError (Decimal 3)
-- -- Right 5.060
-- --
-- decimal ::
--      forall s x y. (KnownNat s, Integral x, Integral y)
--   => x
--   -> y
--   -> Either DecimalError (Decimal s)
-- decimal whole frac
--   | whole /= 0 && 18 < s =
--     Left $ makeArithError Overflow "decimal" ("Scaling factor is too big " ++ show s)
--   | otherwise = do
--     x@(Decimal w) <- fromIntegralDecimal whole
--     y@(Decimal f) <- toDecimal frac
--     if f > scale
--       then Left $
--            makeArithError
--              Overflow
--              "decimal"
--              ("Fractional part " ++
--               show (toInteger frac) ++ " doesn't fit into available decimal places: " ++ show s)
--       else x +$ y
--   where
--     scale = 10 ^ s :: Int64
--     !s = getScale (Proxy :: Proxy s)


-- fromIntegralDecimal ::
--      forall s a. (KnownNat s, Integral p)
--   => a
--   -> Either DecimalError (Decimal s p)
-- fromIntegralDecimal whole
--   | fromIntegral (maxDecimal `div` scale) < whole =
--     Left $
--     makeArithError
--       Overflow
--       "fromIntegralDecimal"
--       ("Number is too big for the Decimal64 " ++ show (toInteger whole))
--   | otherwise = toDecimal (fromIntegral whole * scale)
--   where
--     !scale = 10 ^ getScale (Proxy :: Proxy s) :: Int64

--addDecimal (Decimal x) (Decimal y)

sumDecimal :: (Bounded p, Num p, Ord p, Show p, Foldable t) => t (Decimal r s p) -> ArithM (Decimal r s p)
sumDecimal = foldM (liftM2Decimal plusBounded) (Decimal 0)


instance Bounded p => Bounded (Decimal r s p) where
  minBound = Decimal minBound
  maxBound = Decimal maxBound

liftDecimal :: (p -> p) -> Decimal r s p -> Decimal r s p
liftDecimal f (Decimal x) = Decimal (f x)

liftMDecimal :: Functor f => (p -> f p) -> Decimal r s p -> f (Decimal r s p)
liftMDecimal f (Decimal x) = fmap Decimal (f x)

liftM2Decimal :: Functor f => (p -> p -> f p) -> Decimal r s p -> Decimal r s p -> f (Decimal r s p)
liftM2Decimal f (Decimal x) (Decimal y) = fmap Decimal (f x y)


bind2DecimalM ::
     Monad m => (p1 -> p2 -> m p) -> m (Decimal r1 s1 p1) -> m (Decimal r2 s2 p2) -> m (Decimal r s p)
bind2DecimalM f dx dy = do
  Decimal x <- dx
  Decimal y <- dy
  Decimal <$> f x y

instance (Integral p, Show p, Bounded p, Num p, KnownNat s) => Num (ArithM (Decimal RoundHalfUp s p)) where
  (+) = bind2DecimalM plusBounded
  (-) = bind2DecimalM minusBounded
  (*) adx ady = roundHalfUp <$> do
    dx <- adx
    dy <- ady
    dx *$ dy
  signum = fmap (liftDecimal signum)
  abs = fmap (liftDecimal abs)
  fromInteger i = Decimal <$> fromIntegerBounded i


-- | Add two decimal numbers while checking for `Overflow`
--(+$) :: Decimal s -> Decimal s -> Either DecimalError (Decimal s)
add :: (Num p, Ord p, Show p, Bounded p) => Decimal r s p -> Decimal r s p -> ArithM (Decimal r s p)
add (Decimal x) (Decimal y) = Decimal <$> plusBounded x y


-- -- | Type restricted synonym for `decimal`.
-- (.$) :: KnownNat s => Int -> Int -> Either DecimalError (Decimal s)
-- (.$) = decimal

-- -- | Add two decimal numbers while checking for `Overflow`
-- (+$) :: Decimal s -> Decimal s -> Either DecimalError (Decimal s)
-- (+$) (Decimal x) (Decimal y)
--   | maxDecimal - x < y =
--     Left $
--     makeArithError
--       Overflow
--       "(+$)"
--       ("Sum of " ++ show x ++ " + " ++ show y ++ " is larger than the maximum supported decimal")
--   | otherwise = Right $ Decimal (x + y)


-- -- | Subtract one decimal number from another while validating that the result will not `Underflow``
-- (-$) :: Decimal s -> Decimal s -> Either DecimalError (Decimal s)
-- (-$) (Decimal x) (Decimal y)
--   | x < y =
--     Left $
--     makeArithError
--       Overflow
--       "(+$)"
--       (show x ++ " is less then " ++ show y ++ ". Negative decimal numbers are not supported")
--   | otherwise = Right $ Decimal (x - y)


-- | Multiply two decimal numbers, adjusting their scale at the type level as well.
(*$) ::
     (Num p, Ord p, Integral p, Bounded p)
  => Decimal r m p
  -> Decimal r n p
  -> ArithM (Decimal r (m + n) p)
(*$) (Decimal x) (Decimal y) = Decimal <$> timesBounded x y


-- -- | Just like `sum` but for `Decimal`.
-- sumDecimal :: Foldable t => t (Decimal s) -> Either DecimalError (Decimal s)
-- sumDecimal = foldM (+$) (Decimal 0)


-- quotRemDecimal :: Decimal s -> Int -> Either DecimalError (Decimal s, Decimal s)
-- quotRemDecimal (Decimal x) y
--   | y == 0 =
--     Left $
--     makeArithError
--       DivideByZero
--       "quotRemDecimal"
--       ("Division by zero")
--   | y < 0 =
--     Left $
--     makeArithError
--       Underflow
--       "quotRemDecimal"
--       ("Division by a negative number: " ++ show y)
--   | otherwise =
--     Right $ (Decimal *** Decimal) (x `quotRem` fromIntegral y)


-- | Apply standard mathematical round half up (i.e. [0..4] down, [5..9] up)
roundHalfUp ::
     forall p n k. (Integral p, KnownNat k)
  => Decimal RoundHalfUp (n + k) p
  -> Decimal RoundHalfUp n p
roundHalfUp (Decimal x)
  | k == 0               = Decimal x
  | r < 5 * 10 ^ (k - 1) = Decimal q
  | otherwise            = Decimal (q + 1)
  where
    k = fromIntegral (natVal (Proxy :: Proxy k)) :: Word
    (q, r) = quotRem x (10 ^ k)


instance (Integral p, KnownNat s) => Show (Decimal r s p) where
  show d@(Decimal a)
    | s == 0 = show $ toInteger a
    | r == 0 = printf ("%u." ++ replicate s '0') $ toInteger q
    | otherwise = printf fmt (toInteger q) (toInteger r)
    where s = getScale d
          fmt = "%u.%0" ++ show s ++ "u"
          (q, r) = quotRem a (10 ^ s)

-- -- | Multiplication follows common round ur s ptrategy, therefore it is no longer associative.
-- instance KnownNat s => Num (Decimal s) where
--   (+) x y = either throw id (x +$ y)
--   (-) x y = either throw id (x -$ y)
--   (*) x y = roundHalfUp (either throw id (x *$ y))
--   negate x = throw $ negativeDecimalError "negate" x
--   abs = id
--   signum _ = 1
--   fromInteger = either throw id . fromIntegralDecimal

-- negativeDecimalError :: Show a => String -> a -> DecimalError
-- negativeDecimalError fName x =
--   makeArithError Underflow fName ("Negative decimal numbers are not supported: " ++ show x)
