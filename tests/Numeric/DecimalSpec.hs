{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Numeric.DecimalSpec (spec) where

import           Control.DeepSeq
import           Control.Exception       hiding (assert)
import           Data.Either
import           Data.Int
import           Data.Proxy
import           Data.Scientific
import           Data.Typeable
import           Data.Word
import           GHC.TypeLits
import           Numeric.Decimal
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

-- | Values generated will usually be somewhere close to the bounds.
newtype Extremum a = Extremum a deriving Show

instance (Arbitrary a, Bounded a, Integral a) => Arbitrary (Extremum a) where
  arbitrary = do
    x <- arbitrary
    frequency $
      [(f, pure (Extremum v)) | (f, v) <- [(40, minBound - x), (40, maxBound - x), (20, x)]]

instance (Arbitrary p) => Arbitrary (Decimal r s p) where
  arbitrary = fmap pure arbitrary

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

prop_divBounded ::
     (Arbitrary a, Show a, Integral a, Bounded a, NFData a)
  => Extremum a
  -> Extremum a
  -> Property
prop_divBounded (Extremum x) (Extremum y) =
  classify (isLeft resBounded) "Received Exception" $
  case resBounded of
    Left exc  -> assertException (==exc) (x `div` y)
    Right res -> res === x `div` y
  where
    resBounded = divBounded x y

prop_quotBounded ::
     (Arbitrary a, Show a, Integral a, Bounded a, NFData a)
  => Extremum a
  -> Extremum a
  -> Property
prop_quotBounded (Extremum x) (Extremum y) =
  classify (isLeft resBounded) "Received Exception" $
  case resBounded of
    Left exc  -> assertException (==exc) (x `quot` y)
    Right res -> res === x `quot` y
  where
    resBounded = quotBounded x y


specBouned ::
     forall a. (Typeable a, Arbitrary a, Show a, Integral a, Bounded a, NFData a)
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
    it "divBounded" $
      property (prop_divBounded :: Extremum a -> Extremum a -> Property)
    it "quotBounded" $
      property (prop_quotBounded :: Extremum a -> Extremum a -> Property)


showType :: forall t . Typeable t => Proxy t -> String
showType _ = (showsTypeRep (typeRep (Proxy :: Proxy t))) ""

prop_toFromScientific ::
     (Arbitrary p, Integral p, KnownNat s) => Proxy s -> Proxy (r, p) -> Decimal r s p -> Property
prop_toFromScientific _ _ d =
  (Right d === (fmap fromInteger <$> fromScientific (toScientific d))) .&&.
  (Right d === (fmap fromInteger <$> fromScientific (normalize (toScientific d))))

prop_toFromScientificBounded ::
     (Arbitrary p, Integral p, Bounded p, KnownNat s)
  => Proxy s
  -> Proxy (r, p)
  -> Decimal r s p
  -> Property
prop_toFromScientificBounded _ _ d =
  (Right d === (fromScientificBounded (toScientific d))) .&&.
  (Right d === (fromScientificBounded (normalize (toScientific d))))


specDecimal ::
     forall r s p. (Typeable r, Typeable p, KnownNat s, Integral p, Bounded p, Arbitrary p)
  => Proxy s
  -> Proxy (r, p)
  -> Spec
specDecimal ps px = do
  describe
    ("Decimal " <> showType (Proxy :: Proxy r) <> " " <> show (natVal ps) <> " " <>
     showType (Proxy :: Proxy p)) $ do
    it "toFromScientific" $ property $ prop_toFromScientific ps px
    it "toFromScientificBounded" $ property $ prop_toFromScientificBounded ps px

spec :: Spec
spec = do
  describe "Int" $ do
    specBouned (Proxy :: Proxy Int)
    specBouned (Proxy :: Proxy Int8)
    specBouned (Proxy :: Proxy Int16)
    specBouned (Proxy :: Proxy Int32)
    specBouned (Proxy :: Proxy Int64)
    specDecimal (Proxy :: Proxy 4) (Proxy :: Proxy (RoundHalfUp, Int))
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
