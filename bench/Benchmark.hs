{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE PackageImports #-}
module Main where

import           Control.Exception
import           Control.Monad
import           Criterion.Main
import           Data.Coerce
import qualified "Decimal" Data.Decimal               as Decimal
import           Data.Foldable
import           Data.Int
import           Data.Maybe
import           Data.Ratio
import           Data.Scientific
import qualified "decimal-arithmetic" Numeric.Decimal as DecimalArithmetic
import qualified "safe-decimal" Numeric.Decimal       as SafeDecimal
import           System.Random                        (mkStdGen, randoms)

type SafeDecimal = SafeDecimal.Decimal SafeDecimal.RoundHalfUp 5 Int64

tryArith :: Either ArithException SafeDecimal -> SafeDecimal
tryArith = either throw id

toSafeDecimal :: Integer -> SafeDecimal
toSafeDecimal = tryArith . fromInteger


toDecimalRaw :: Integer -> Decimal.DecimalRaw Int
toDecimalRaw = fromJust . Decimal.decimalConvert . Decimal.Decimal 5


toDecimalArithmetic :: Integer -> DecimalArithmetic.Decimal64
toDecimalArithmetic = fromRational . (% 100000)


sumSafeDecimal :: (Functor t, Foldable t) => t SafeDecimal -> SafeDecimal
sumSafeDecimal = tryArith . sum . fmap Right

sample :: [Int32]
sample =
  let initialSeed, num :: Int
      initialSeed = 2018
      num = 500000
   in take num (randoms $ mkStdGen initialSeed)


main :: IO ()
main = do
  let ls = map (toInteger . abs) sample -- positive integers [0..2^32]
  defaultMain
    [ bgroup
        "wrap Integer"
        [ env (return ls) (bench "decimal64" . nf (map toSafeDecimal))
        , env (return ls) (bench "Decimal" . nf (map toDecimalRaw))
        , env (return ls) (bench "decimal-arithmetic" . nf (map toDecimalArithmetic))
        ]
    , bgroup
        "fromInteger"
        [ env (return ls) (bench "decimal64" . nf (map toSafeDecimal))
        , env
            (return ls)
            (bench "Decimal" . nf (map (fromInteger :: Integer -> Decimal.DecimalRaw Int)))
        , env
            (return ls)
            (bench "decimal-arithmetic" .
             nf (map (fromInteger :: Integer -> DecimalArithmetic.Decimal64)))
        ]
    -- , bgroup
    --     "Decimal from two Ints"
    --     [ env (return ls) (bench "decimal64" . nf (map toSafeDecimal))
    --     , env (return ls) (bench "Decimal" . nf (map toDecimalRaw))
    --     , env (return ls) (bench "decimal-arithmetic" . nf (map toDecimalArithmetic))
    --     ]
    , bgroup
        "Sum"
          -- env (return ls) (bench "Integer'" . nf (foldl' (+) 0))
        -- ,
        [ env
            (return ls)
            (bench "[Integer]" . nf (foldl' (+) 0))
        , env
            (return (map (fromInteger :: Integer -> Int64) ls))
            (bench "[Int64]" . nf (foldl' (+) 0))
        , env
            (return (map (`scientific` 5) ls))
            (bench "[Scientific]" . nf (foldl' (+) 0))
        , env
            (return
               ((coerce :: [Integer] -> [SafeDecimal.Decimal SafeDecimal.RoundHalfUp 5 Integer]) ls))
            (bench "[SafeDecimal]/Integer" . nf (foldl' (+) 0))
        , env (return (map toSafeDecimal ls)) (bench "[SafeDecimal]/Int64" . nf sumSafeDecimal)
        , env (return (map toDecimalRaw ls)) (bench "[Decimal]/Decimal" . nf (foldl' (+) 0))
        , env
            (return (map toDecimalArithmetic ls))
            (bench "[Decimal]/decimal-arithmetic" . nf (foldl' (+) 0))
        ]
    ]
