module Main (main) where

import Criterion.Main (bgroup, defaultMain)
import qualified Problem9Bench

main :: IO ()
main = defaultMain
    [ bgroup "Problem9" Problem9Bench.benchmarks
    ]