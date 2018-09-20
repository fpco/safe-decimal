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
  , fromScientific
  , toScientific
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
  liftA2 = liftDecimal2
  {-# INLINABLE liftA2 #-}


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
  fromInteger = fmap Decimal . fromIntegerBounded
  {-# INLINABLE fromInteger #-}


-- | Add two bounded numbers while checking for `Overflow`/`Underflow`
plusBounded :: (Integral a, Bounded a) => a -> a -> Either ArithException a
plusBounded x y
  | (x > 0 && y > 0 && x > maxBound - y) = Left Overflow
  | (x < 0 && y < 0 && x < minBound - y) = Left Underflow
  | otherwise = Right (x + y)
{-# INLINABLE plusBounded #-}

-- | Subtract two bounded numbers while checking for `Overflow`/`Underflow`
minusBounded :: (Integral a, Bounded a) => a -> a -> Either ArithException a
minusBounded x y
  | (x > 0 && y < 0 && x > maxBound + y) = Left Overflow
  | (x < 0 && y > 0 && x < minBound + y) = Left Underflow
  | otherwise = Right (x - y)
{-# INLINABLE minusBounded #-}


-- | Add two decimal numbers while checking for `Overflow`
timesBounded :: (Integral a, Bounded a) => a -> a -> Either ArithException a
timesBounded x y
  | y == 0 = Right 0
  | (x < 0 && y == minBound) ||
  ----------- ^ Here we deal with Overflow when (minBound * (-1))
      (y > 0 && maxBoundDivY < x) || (y < 0 && x < maxBoundDivY) = Left Overflow
  | (y > 0 && minBoundDivY > x) || (y + 1 < 0 && x > minBoundDivY) = Left Underflow
  ----------------------------------- ^ Here we also deal with Overflow when (minBound * (-1))
  | otherwise = Right (x * y)
  where
    maxBoundDivY = maxBound `div` y
    minBoundDivY = minBound `div` y
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


-- | Multiply two decimal numbers, adjusting their scale at the type level as well.
timesDecimal ::
     (Integral p, Bounded p)
  => Decimal r s1 p
  -> Decimal r s2 p
  -> Either ArithException (Decimal r (s1 + s2) p)
timesDecimal (Decimal x) (Decimal y) = Decimal <$> timesBounded x y
{-# INLINABLE timesDecimal #-}


-- type family Max (s1 :: Nat) (s2 :: Nat) where
--   Max 0 b = b
--   Max a b = If (a <=? b) b a

-- -- | Multiply two decimal numbers, adjusting their scale at the type level as well.
-- plusDecimal ::
--      forall r p s s1 s1 . (Num p, Ord p, Integral p, Bounded p, s ~ (Max s1 s2))
--   => Decimal r s1 p
--   -> Decimal r s2 p
--   -> Either ArithException (Decimal r s p)
-- plusDecimal (Decimal x) (Decimal y) = Decimal <$> plusBounded x y
-- {-# INLINABLE plusDecimal #-}



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



instance (Integral p, KnownNat s) => Show (Decimal r s p) where
  show d@(Decimal a)
    | s == 0 = show $ toInteger a
    | r == 0 = printf ("%u." ++ replicate s '0') $ toInteger q
    | otherwise = printf fmt (toInteger q) (toInteger r)
    where s = getScale d
          fmt = "%u.%0" ++ show s ++ "u"
          (q, r) = quotRem a (10 ^ s)




-- sumDecimal :: (Bounded p, Num p, Ord p, Show p, Foldable t) => t (Decimal r s p) -> ArithM (Decimal r s p)
-- sumDecimal = foldM (liftM2Decimal plusBounded) (Decimal 0)


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



---- Scientific

toScientific :: (Integral p, KnownNat s) => Decimal r s p -> Scientific
toScientific dec = scientific (toInteger (unwrapDecimal dec)) (negate (getScale dec))


fromScientific :: forall r s . KnownNat s => Scientific -> Maybe (Decimal r s Integer)
fromScientific num
  | point10 > s = Nothing
  | otherwise = Just (fromNum (coeff ^ (s - point10)))
  where
      s = natVal (Proxy :: Proxy s)
      numNormal = normalize num
      coeff = coefficient numNormal
      point10 = toInteger (negate (base10Exponent numNormal))

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


-- instance KnownNat s => Show (Decimal s) where
--   show (Decimal a)
--     | s == 0 = show a
--     | r == 0 = printf ("%u." ++ replicate s '0') q
--     | otherwise = printf fmt q r
--     where s = fromIntegral (natVal (Proxy :: Proxy s)) :: Int
--           fmt = "%u.%0" ++ show s ++ "u"
--           (q, r) = quotRem a (10 ^ s)

-- showDecimal :: KnownNat s => Decimal s -> T.Text
-- showDecimal = T.pack . show

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
