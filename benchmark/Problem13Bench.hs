module Problem13Bench (benchmarks) where

import Criterion (Benchmark, bench, whnf)
import DailyCodingProblem.Problem13.Solution
import qualified Data.Vector as V

chars = "aabcchbrejffjokttooieieijfcosirjsierugvnsmhgsdcmmfffhueeopwljvauhcgserg\
        \aabcchbrejffjokttooieieijfcosirjsierugvnsmhgsdcmmfffhueeopwljvauhcgserg\
        \aabcchbrejffjokttooieieijfcosirjsierugvnsmhgsdcmmfffhueeopwljvauhcgserg"

benchmarks :: [Benchmark]
benchmarks =
    [ bench "longestKSLength" $ whnf (longestKSLength 50) chars
    , bench "longestKSLength'" $ whnf (longestKSLength' 50) (V.fromList chars)
    ]