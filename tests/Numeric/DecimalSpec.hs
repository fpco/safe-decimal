{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.DecimalSpec (spec) where

import           Control.Exception
import           Data.Int
import           Data.Proxy
import           Data.Typeable
import           Data.Word
import           Numeric.Decimal
import           Test.Hspec
import           Test.QuickCheck

newtype Extremum a = Extremum a deriving Show

instance (Arbitrary a, Bounded a, Integral a) => Arbitrary (Extremum a) where
  arbitrary = do
    x <- arbitrary
    frequency $
      [(f, pure (Extremum v)) | (f, v) <- [(40, minBound - x), (40, maxBound - x), (20, x)]]

prop_plusBounded ::
     (Arbitrary a, Show a, Integral a, Bounded a)
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
    resBounded = plusBounded x y


prop_minusBounded ::
     (Arbitrary a, Show a, Integral a, Bounded a)
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
    resBounded = minusBounded x y

prop_timesBounded ::
     (Arbitrary a, Show a, Integral a, Bounded a)
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
    resBounded = timesBounded x y

prop_fromIntegerBounded ::
  forall a . (Arbitrary a, Show a, Integral a, Bounded a)
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
    resBounded = fromIntegerBounded x' :: Either ArithException a


specBouned ::
     forall a. (Typeable a, Arbitrary a, Show a, Integral a, Bounded a)
  => Proxy a
  -> Spec
specBouned px = do
  let typeName = showsTypeRep (typeRep px) ""
  describe ("Bounded: " ++ typeName) $ do
    let excs = [Overflow, Underflow]
        plusExcs = if (minBound :: a) >= 0 then [Overflow] else excs
    it "plusBounded" $ property (prop_plusBounded plusExcs :: Extremum a -> Extremum a -> Property)
    it "minusBounded" $
      property (prop_minusBounded excs :: Extremum a -> Extremum a -> Property)
    it "timesBounded" $
      property (prop_timesBounded excs :: Extremum a -> Extremum a -> Property)
    it "fromIntegerBounded" $
      property (prop_fromIntegerBounded excs :: Int -> Extremum a -> Property)

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
