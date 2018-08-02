{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE PackageImports #-}
module Main where

import           Control.Exception
import           Control.Monad
import           Criterion.Main
import qualified "Decimal" Data.Decimal               as Decimal
import           Data.Foldable
import           Data.Int
import           Data.Maybe
import           Data.Ratio
import           Numeric.ArithM
import qualified "safe-decimal" Numeric.Decimal            as SafeDecimal
import qualified "decimal-arithmetic" Numeric.Decimal as DecimalArithmetic
import           System.Random                        (mkStdGen, randoms)

type SafeDecimal' = SafeDecimal.Decimal64 SafeDecimal.RoundHalfUp 5

toSafeDecimal :: Integer -> SafeDecimal'
toSafeDecimal = SafeDecimal.toDecimal . tryArithM . fromInteger


toDecimalRaw :: Integer -> Decimal.DecimalRaw Int
toDecimalRaw = fromJust . Decimal.decimalConvert . Decimal.Decimal 5


toDecimalArithmetic :: Integer -> DecimalArithmetic.Decimal64
toDecimalArithmetic = fromRational . (% 100000)


sumSafeDecimal' :: Foldable t => t SafeDecimal' -> SafeDecimal'
sumSafeDecimal' = tryArithM . SafeDecimal.sumDecimal

sample :: [Int32]
sample =
  let initialSeed, num :: Int
      initialSeed = 2018
      num = 500000
   in take num (randoms $ mkStdGen initialSeed)


main :: IO ()
main = do
  let ls = map (toInteger . abs) sample
  defaultMain
    [ bgroup
        "wrap Integer"
        [ env (return ls) (bench "decimal64" . nf (map toSafeDecimal))
        , env (return ls) (bench "Decimal" . nf (map toDecimalRaw))
        , env (return ls) (bench "decimal-arithmetic" . nf (map toDecimalArithmetic))
        ]
    , bgroup
        "fromInteger"
        [ env
            (return ls)
            (bench "decimal64" . nf (map (fromInteger :: Integer -> ArithM SafeDecimal')))
        , env
            (return ls)
            (bench "decimal64'" .
             nf (map (tryArithM . (fromInteger :: Integer -> ArithM SafeDecimal'))))
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
            (return (map (fromInteger :: Integer -> Int64) ls))
            (bench "[Int64]" . nf (foldl' (+) 0))
        , env (return (map toSafeDecimal ls)) (bench "[Decimal]/decimal64" . nf sumSafeDecimal')
        , env (return (map toDecimalRaw ls)) (bench "[Decimal]/Decimal" . nf (foldl' (+) 0))
        , env
            (return (map toDecimalArithmetic ls))
            (bench "[Decimal]/decimal-arithmetic" . nf (foldl' (+) 0))
        ]
    ]
