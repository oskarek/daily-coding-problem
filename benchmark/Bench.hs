module Main (main) where

import Criterion.Main (bgroup, defaultMain)
import qualified Problem9Bench
import qualified Problem11Bench
import qualified Problem12Bench

main :: IO ()
main = defaultMain
    [ bgroup "Problem9" Problem9Bench.benchmarks
    , bgroup "Problem11" Problem11Bench.benchmarks
    , bgroup "Problem12" Problem12Bench.benchmarks
    ]