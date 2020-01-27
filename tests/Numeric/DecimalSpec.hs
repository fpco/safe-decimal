{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Numeric.DecimalSpec (spec) where

import Control.DeepSeq
import Control.Exception hiding (assert)
import Control.Monad
import Data.Either
import Data.Int
import Data.Proxy
import Data.Ratio
import Data.Scientific
import Data.Typeable
import Data.Word
import GHC.TypeLits
import Numeric.Decimal
import Numeric.Natural
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

-- | Values generated will usually be somewhere close to the bounds.
newtype Extremum a = Extremum a deriving Show

instance (Arbitrary a, Bounded a, Integral a) => Arbitrary (Extremum a) where
  arbitrary = do
    NonNegative x <- arbitrary
    frequency
      [ (f, pure (Extremum v))
      | (f, v) <- [(40, minBound + x), (40, maxBound - x), (20, x)]
      ]

instance (Arbitrary p) => Arbitrary (Decimal r s p) where
  arbitrary = fmap pure arbitrary

showType :: forall t . Typeable t => Proxy t -> String
showType _ = showsTypeRep (typeRep (Proxy :: Proxy t)) ""


prop_plusBounded ::
     (Show a, Integral a, Bounded a)
  => [ArithException] -- ^ Exceptions to expect
  -> Extremum a
  -> Extremum a
  -> Property
prop_plusBounded excs (Extremum x) (Extremum y) =
  classify (not withinBounds) "Outside of Bounds" $
  if withinBounds
    then Right res === resBounded
    else disjoin (fmap ((resBounded ===) . Left) excs)
  where
    res = x + y
    withinBounds = toInteger res == toInteger x + toInteger y
    resBounded = toArithException $ plusBounded x y


prop_minusBounded ::
     (Show a, Integral a, Bounded a)
  => [ArithException] -- ^ Exceptions to expect
  -> Extremum a
  -> Extremum a
  -> Property
prop_minusBounded excs (Extremum x) (Extremum y) =
  classify (not withinBounds) "Outside of Bounds" $
  if withinBounds
    then Right res === resBounded
    else disjoin (fmap ((resBounded ===) . Left) excs)
  where
    res = x - y
    withinBounds = toInteger res == toInteger x - toInteger y
    resBounded = toArithException $ minusBounded x y

prop_timesBounded ::
     (Show a, Integral a, Bounded a)
  => [ArithException] -- ^ Exceptions to expect
  -> Extremum a
  -> Extremum a
  -> Property
prop_timesBounded excs (Extremum x) (Extremum y) =
  classify (not withinBounds) "Outside of Bounds" $
  if withinBounds
    then Right res === resBounded
    else disjoin (fmap ((resBounded ===) . Left) excs)
  where
    res = x * y
    withinBounds = toInteger res == toInteger x * toInteger y
    resBounded = toArithException $ timesBounded x y

prop_fromIntegerBounded ::
  forall a . (Show a, Integral a, Bounded a)
  => [ArithException] -- ^ Exceptions to expect
  -> Int -- ^ This is used for scaling
  -> Extremum a
  -> Property
prop_fromIntegerBounded excs n (Extremum x) =
  classify (not withinBounds) "Outside of Bounds" $
  if withinBounds
    then Right (fromInteger x') === resBounded
    else disjoin (fmap ((resBounded ===) . Left) excs)
  where
    multiplier = (n `mod` 3) + 1
    x' = toInteger x * toInteger multiplier -- Try to go overboard 66% of the time
    withinBounds = x' == toInteger (x * fromIntegral multiplier)
    resBounded :: Either ArithException a
    resBounded = toArithException $ fromIntegerBounded x'

-- | Throw all exceptions except the ArithException
toArithException :: Either SomeException a -> Either ArithException a
toArithException eRes =
  case eRes of
    Left exc
      | Just arithExc <- fromException exc -> Left arithExc
    Left exc -> throw exc
    Right res -> Right res

prop_divBounded ::
     (Show a, Integral a, Bounded a, NFData a)
  => Extremum a
  -> Extremum a
  -> Property
prop_divBounded (Extremum x) (Extremum y) =
  classify (isLeft resBounded) "Received Exception" $
  case resBounded of
    Left exc  -> assertException (==exc) (x `div` y)
    Right res -> res === x `div` y
  where
    resBounded = toArithException $ divBounded x y

prop_quotBounded ::
     (Show a, Integral a, Bounded a, NFData a)
  => Extremum a
  -> Extremum a
  -> Property
prop_quotBounded (Extremum x) (Extremum y) =
  classify (isLeft resBounded) "Received Exception" $
  case resBounded of
    Left exc  -> assertException (==exc) (x `quot` y)
    Right res -> res === x `quot` y
  where
    resBounded = toArithException $ quotBounded x y


specBouned ::
     forall a. (Typeable a, Arbitrary a, Show a, Integral a, Bounded a, NFData a)
  => Proxy a
  -> Spec
specBouned px = do
  let typeName = showsTypeRep (typeRep px) ""
  describe ("Bounded: " ++ typeName) $ do
    let excs = [Overflow, Underflow]
        plusExcs =
          if (minBound :: a) >= 0
            then [Overflow]
            else excs
    it "plusBounded" $
      property
        (prop_plusBounded plusExcs :: Extremum a -> Extremum a -> Property)
    it "minusBounded" $
      property (prop_minusBounded excs :: Extremum a -> Extremum a -> Property)
    it "timesBounded" $
      property (prop_timesBounded excs :: Extremum a -> Extremum a -> Property)
    it "fromIntegerBounded" $
      property (prop_fromIntegerBounded excs :: Int -> Extremum a -> Property)
    it "divBounded" $
      property (prop_divBounded :: Extremum a -> Extremum a -> Property)
    it "quotBounded" $
      property (prop_quotBounded :: Extremum a -> Extremum a -> Property)
  specBoundedDecimal (Proxy :: Proxy RoundHalfUp) (Proxy :: Proxy 0) px
  specBoundedDecimal (Proxy :: Proxy RoundHalfUp) (Proxy :: Proxy 1) px
  specBoundedDecimal (Proxy :: Proxy RoundHalfUp) (Proxy :: Proxy 2) px
  let maxLen = length (show (maxBound :: a))
  when (maxLen >= 3) $
    specBoundedDecimal (Proxy :: Proxy RoundHalfUp) (Proxy :: Proxy 3) px
  when (maxLen >= 4) $
    specBoundedDecimal (Proxy :: Proxy RoundHalfUp) (Proxy :: Proxy 4) px
  when (maxLen >= 5) $
    specBoundedDecimal (Proxy :: Proxy RoundHalfUp) (Proxy :: Proxy 5) px
  when (maxLen >= 19) $
    specBoundedDecimal (Proxy :: Proxy RoundHalfUp) (Proxy :: Proxy 19) px
  it "rounding" $
    property $ \(d :: Decimal RoundHalfUp 1 a) ->
      let r = toRationalDecimal d
       in (roundDecimal d :: Decimal RoundHalfUp 0 a) ===
          throwDecimal (fromRationalDecimalBounded (roundHalfUpTo 0 r))

throwDecimal :: Either SomeException (Decimal r s p) -> Decimal r s p
throwDecimal = either throw id

specBoundedDecimal ::
     forall r s p. (Typeable r, Typeable p, KnownNat s, Show p, Integral p, Bounded p, Arbitrary p)
  => Proxy r
  -> Proxy s
  -> Proxy p
  -> Spec
specBoundedDecimal pr ps pp =
  describe
    ("Decimal " ++ showType (Proxy :: Proxy r) ++ " " ++ show (natVal ps) ++ " " ++
     showType (Proxy :: Proxy p)) $ do
    it "toFromScientific" $ property $ prop_toFromScientific pr ps pp
    it "toFromScientificBounded" $ property $ prop_toFromScientificBounded pr ps pp
    it "showParseBounded" $ property $ prop_showParseBouded pr ps pp
    -- TODO: x times integral / integral == x

prop_toFromScientific ::
     (Integral p, KnownNat s)
  => Proxy r
  -> Proxy s
  -> Proxy p
  -> Decimal r s p
  -> Property
prop_toFromScientific _ _ _ d =
  (Right d === toArithException (fmap fromInteger <$> fromScientific (toScientific d))) .&&.
  (Right d === toArithException (fmap fromInteger <$> fromScientific (normalize (toScientific d))))

prop_toFromScientificBounded ::
     (Integral p, Bounded p, KnownNat s)
  => Proxy r
  -> Proxy s
  -> Proxy p
  -> Decimal r s p
  -> Property
prop_toFromScientificBounded _ _ _ d =
  (Right d === toArithException (fromScientificBounded (toScientific d))) .&&.
  (Right d === toArithException (fromScientificBounded (normalize (toScientific d))))

prop_showParseBouded ::
     (Show p, Integral p, Bounded p, KnownNat s)
  => Proxy r
  -> Proxy s
  -> Proxy p
  -> Decimal r s p
  -> Property
prop_showParseBouded _ _ _ d@(Decimal x) =
  case parseDecimalBounded False (show d) of
    Left err              -> error err
    Right d'@(Decimal x') -> x === x' .&&. d === d'

spec :: Spec
spec = do
  describe "Int" $ do
    specBouned (Proxy :: Proxy Int)
    specBouned (Proxy :: Proxy Int8)
    specBouned (Proxy :: Proxy Int16)
    specBouned (Proxy :: Proxy Int32)
    specBouned (Proxy :: Proxy Int64)
  describe "Word" $ do
    specBouned (Proxy :: Proxy Word)
    specBouned (Proxy :: Proxy Word8)
    specBouned (Proxy :: Proxy Word16)
    specBouned (Proxy :: Proxy Word32)
    specBouned (Proxy :: Proxy Word64)




assertException :: (NFData a, Exception exc) =>
                   (exc -> Bool) -- ^ Return True if that is the exception that was expected
                -> a -- ^ Value that should throw an exception, when fully evaluated
                -> Property
assertException isExc action = assertExceptionIO isExc (return action)


assertExceptionIO :: (NFData a, Exception exc) =>
                     (exc -> Bool) -- ^ Return True if that is the exception that was expected
                  -> IO a -- ^ IO Action that should throw an exception
                  -> Property
assertExceptionIO isExc action =
  monadicIO $ do
    hasFailed <-
      run
        (catch
           (do res <- action
               res `deepseq` return False) $ \exc ->
           show exc `deepseq` return (isExc exc))
    assert hasFailed

roundHalfUpTo :: Natural -> Rational -> Rational
roundHalfUpTo to rational =
  floor ((truncate (rational * ((s10 * 10) % 1) + 5) :: Integer) % 10) % s10
  where
    s10 = 10 ^ to :: Integer

-- Use for testing once HalfAwayFromZero is implemented
_roundCommercial :: Natural -> Rational -> Rational
_roundCommercial to rational
  | rational < 0 = negate (roundPositive (negate rational))
  | otherwise = roundPositive rational
  where
    s10 = 10 ^ to :: Integer
    roundPositive positiveRational =
      let (q, r) = quotRem (truncate (positiveRational * (s10 * 10 % 1)) :: Integer) 10
       in (if r >= 5
             then q + 1
             else q) %
          s10

-- Once round half even is implemented, this can be used for testing
_roundBankerTo :: Integer -> Rational -> Rational
_roundBankerTo to rational =
  fromInteger (round $ rational * (10 ^ to)) / 10 ^ to
