{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Numeric.ArithM where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Data.Int

newtype ArithError =
  ArithException [(ArithException, String)]
  deriving (Monoid, Semigroup, Show, Eq)

instance Exception ArithError


data ArithM r
  = ArithResult !r
  | ArithError !ArithError
  deriving Show

instance NFData r => NFData (ArithM r) where
  rnf (ArithResult r) = r `deepseq` ()
  rnf (ArithError err) = err `seq` ()

-- | Pull out the result of arithmetic operations, while falling back onto a default value if
-- arithmetic computation resulted in an error
fromArithM :: r -> ArithM r -> r
fromArithM _ (ArithResult r) = r
fromArithM r (ArithError _)  = r

-- | Convert Arithmetic computation into an `Either`
eitherArithM :: ArithM r -> Either ArithError r
eitherArithM (ArithResult r)  = Right r
eitherArithM (ArithError err) = Left err

-- | Convert Arithmetic computation into an `Maybe`
maybeArithM :: ArithM r -> Maybe r
maybeArithM (ArithResult r)  = Just r
maybeArithM (ArithError _err) = Nothing

-- | Pull out the result of arithmetic operations, while throwing an error if was unsuccessfull.
tryArithM :: ArithM r -> r
tryArithM (ArithResult r)  = r
tryArithM (ArithError err) = throw err

instance Functor ArithM where
  fmap f a =
    case a of
      ArithResult r  -> ArithResult (f r)
      ArithError err -> ArithError err

instance Applicative ArithM where
  pure = ArithResult
  (<*>) af a =
    case af of
      ArithResult rf ->
        case a of
          ArithResult r  -> ArithResult (rf r)
          ArithError err -> ArithError err
      ArithError err -> ArithError err

instance Alternative ArithM where
  empty = ArithError mempty
  (<|>) a1@(ArithResult _) _                = a1
  (<|>) _              a2@(ArithResult _)   = a2
  (<|>) (ArithError err1) (ArithError err2) = ArithError (err1 <> err2)

instance Monad ArithM where
  (>>=) m f =
    case m of
      ArithResult r  -> f r
      ArithError err -> ArithError err
  return = pure


makeArithError :: ArithException -> String -> String -> ArithError
makeArithError exc fName err = ArithException [(exc, fName ++ ": " ++ err)]


sumBounded :: (Num a, Ord a, Show a, Bounded a, Foldable t) => t a -> ArithM a
sumBounded = foldM plusBounded 0

-- | Add two bounded numbers while checking for `Overflow`/`Underflow`
plusBounded :: (Num a, Ord a, Show a, Bounded a) => a -> a -> ArithM a
plusBounded x y
  | (y >= 0 && maxBound - y >= x) || (y <= 0 && minBound - y <= x) = ArithResult (x + y)
  | otherwise =
    ArithError $
    makeArithError
      Overflow
      "plusBounded"
      ("Sum of " ++
       show x ++
       " + " ++ show y ++ " is too large or too small for a supported value")


-- | Subtract two bounded numbers while checking for `Overflow`/`Underflow`
minusBounded :: (Num a, Ord a, Show a, Bounded a) => a -> a -> ArithM a
minusBounded x y
  | (y >= 0 && minBound + y <= x) || (y <= 0 && maxBound + y >= x) = ArithResult (x + y)
  | otherwise =
    ArithError $
    makeArithError
      Overflow
      "minBounded"
      ("Minus of " ++
       show x ++
       " + " ++ show y ++ " is too large or too small for a supported value")


-- | Add two decimal numbers while checking for `Overflow`
timesBounded :: (Num a, Ord a, Integral a, Bounded a) => a -> a -> ArithM a
timesBounded x y
  | y == 0 = ArithResult 0
  | y > 0 && (maxBoundDivY >= x && minBoundDivY <= x) ||
      y < 0 && (maxBoundDivY < x && minBoundDivY > x) = ArithResult (x * y)
  | otherwise =
    ArithError $
    makeArithError
      Overflow
      "timesBounded"
      ("Product of " ++
       show (toInteger x) ++
       " * " ++ show (toInteger y) ++ " is too large or too small for a supported value")
  where
    maxBoundDivY = maxBound `div` y
    minBoundDivY = minBound `div` y


fromIntegerBounded :: forall a. (Num a, Ord a, Integral a, Bounded a) => Integer -> ArithM a
fromIntegerBounded x
  | x > toInteger (maxBound :: a) =
    ArithError $ makeArithError Overflow "fromIntegerBounded" ("Number " ++ show x ++ "is too big")
  | x < toInteger (minBound :: a) =
    ArithError $ makeArithError Underflow "fromIntegerBounded" ("Number " ++ show x ++ "is too small")
  | otherwise = pure $ fromInteger x



-- | Same as (join . liftM2)
bindM2 ::
     Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f dx dy = do
  x <- dx
  y <- dy
  f x y


instance Bounded a => Bounded (ArithM a) where
  maxBound = pure maxBound
  minBound = pure minBound

instance Num (ArithM Int) where
  (+) = bindM2 plusBounded
  (-) = bindM2 minusBounded
  (*) = bindM2 timesBounded
  signum = fmap signum
  abs = fmap abs
  fromInteger = fromIntegerBounded


instance Num (ArithM Int64) where
  (+) = bindM2 plusBounded
  (-) = bindM2 minusBounded
  (*) = bindM2 timesBounded
  signum = fmap signum
  abs = fmap abs
  fromInteger = fromIntegerBounded
