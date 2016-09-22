{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.DeepSeq
import Criterion.Main
import Numeric.Units.Dimensional.Prelude
import qualified Prelude as P

main :: IO ()
main = defaultMain [benchmarks]

{-[
         bench "RawArithmetic" $ nf rawArithmetic 1000
       , bench "Arithmetic" $ nf arithmetic 1000
       ]
-}

benchmarks :: Benchmark
benchmarks = env (setupEnv 10000) $ \ ~(ls, fs, xs, xs') -> bgroup "arithmetic" $
             [ bench "Sum" $ nf sum xs
             , bench "RawSum" $ nf P.sum xs'
             ]

setupEnv :: Int -> IO ([Length Double], [Force Double], [Dimensionless Double], [Double])
setupEnv n = do
             let ls = fmap (*~ meter) [1.0 .. fromIntegral n]
             let fs = fmap (*~ kilo newton) [1.0 .. fromIntegral n]
             let xs' = [1.0 .. fromIntegral n]
             let xs = fmap (*~ one) [1.0 .. fromIntegral n]
             return $ (force ls, force fs, force xs, force xs')
