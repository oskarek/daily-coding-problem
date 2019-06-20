module Problem11Bench (benchmarks) where

import Criterion (Benchmark, bench, nf)
import DailyCodingProblem.Problem11.Solution
import qualified Data.Set as S
import Data.List

dictionary = permutations "abcdefghi"
set = S.fromList dictionary
trie = fromList dictionary

benchmarks :: [Benchmark]
benchmarks =
    [ bench "autocomplete_naive" (nf (autocomplete_naive "abeg") set)
    , bench "autocomplete" (nf (autocomplete "abeg") trie)
    ]