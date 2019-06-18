module Problem9Bench (benchmarks) where

import Criterion (Benchmark, bench, whnf)
import DailyCodingProblem.Problem9.Solution

benchmarks :: [Benchmark]
benchmarks =
    [ bench "largestNonAdjacentSum" (whnf largestNonAdjacentSum (replicate 1000000 5))
    ]