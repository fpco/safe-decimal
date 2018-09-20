{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
module Numeric.Decimal
  ( Decimal64
  , RoundHalfUp
  , module Numeric.Decimal.Internal
  -- * Operations
  , decimalList
  , sumDecimal
  , productDecimal
  ) where

import           Control.Exception
import           Control.Monad
import           Data.Int
import           Data.Proxy
import           GHC.TypeLits
import           Numeric.Decimal.Internal
import Data.Coerce

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
      k = fromIntegral (natVal (Proxy :: Proxy k)) :: Word
      (q, r) = quotRem x (10 ^ k)
  {-# INLINABLE roundDecimal #-}

-- | /O(1)/ - Conversion of a list
decimalList :: Integral p => [p] -> [Decimal r s p]
decimalList = coerce

plusDecimal ::
     (Integral p, Bounded p)
  => Decimal r s p
  -> Decimal r s p
  -> Either ArithException (Decimal r s p)
plusDecimal (Decimal x) (Decimal y) = Decimal <$> plusBounded x y
{-# INLINABLE plusDecimal #-}


sumDecimal ::
     (Foldable t, Integral p, Bounded p)
  => t (Decimal r s p)
  -> Either ArithException (Decimal r s p)
sumDecimal = foldM plusDecimal (Decimal 0)
{-# INLINABLE sumDecimal #-}

productDecimal ::
     (KnownNat s, Round r, Integral p, Bounded p)
  => [Decimal r s p]
  -> Either ArithException (Decimal r s p)
productDecimal = foldM (\x y -> roundDecimal <$> timesDecimal x y) (Decimal 1)
{-# INLINABLE productDecimal #-}
