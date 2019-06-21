{-# LANGUAGE OverloadedLists #-}
module Problem12Bench (benchmarks) where

import Criterion (Benchmark, bgroup, bench, whnf)
import DailyCodingProblem.Problem12.Solution

benchmarks :: [Benchmark]
benchmarks =
    [ bgroup "uniqueClimbs"
        [ bench "uniqueClimbs 2" (whnf uniqueClimbs 2)
        , bench "uniqueClimbs 25" (whnf uniqueClimbs 25)
        , bench "uniqueClimbs' 2" (whnf uniqueClimbs' 2)
        , bench "uniqueClimbs' 25" (whnf uniqueClimbs' 25)
        , bench "uniqueClimbs' 300" (whnf uniqueClimbs' 300) ]

    , bgroup "uniqueClimbsN"
        [ bench "uniqueClimbsN {1,3,5} 2" (whnf (uniqueClimbsN [1,3,5]) 2)
        , bench "uniqueClimbsN {1,3,5} 25" (whnf (uniqueClimbsN [1,3,5]) 25)
        , bench "uniqueClimbsN' {1,3,5} 2" (whnf (uniqueClimbsN' [1,3,5]) 2)
        , bench "uniqueClimbsN' {1,3,5} 25" (whnf (uniqueClimbsN' [1,3,5]) 25)
        , bench "uniqueClimbsN' {1,3,5} 300" (whnf (uniqueClimbsN' [1,3,5]) 300)
        , bench "uniqueClimbsN' {1,3,5,14,50,100,104} 300"
            (whnf (uniqueClimbsN' [1,3,5,14,50,100,104]) 300)
        , bench "uniqueClimbsN'' {1,3,5} 2" (whnf (uniqueClimbsN'' [1,3,5]) 2)
        , bench "uniqueClimbsN'' {1,3,5} 25" (whnf (uniqueClimbsN'' [1,3,5]) 25)
        , bench "uniqueClimbsN'' {1,3,5} 300" (whnf (uniqueClimbsN'' [1,3,5]) 300)
        , bench "uniqueClimbsN'' {1,3,5,14,50,100,104} 300"
            (whnf (uniqueClimbsN'' [1,3,5,14,50,100,104]) 300) ]
    ]