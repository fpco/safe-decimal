{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Numeric.Decimal
  ( Decimal64
  , NonNegativeDecimal
  , module           Numeric.Decimal.Internal
  ) where

import           Data.Int
import           Numeric.Decimal.Internal

type Decimal64 r s = Decimal r s Int64

type NonNegativeDecimal r s p = Decimal r s (NonNegative p)


newtype NonNegative a =
  NonNegative a
  deriving (Show, Num, Floating, Fractional, Eq, Ord, Real, Enum, Integral, Functor)


instance (Num a, Bounded a) => Bounded (NonNegative a) where
  maxBound = NonNegative maxBound
  minBound = NonNegative 0
