module DailyCodingProblem.Problem14.Solution where

import qualified System.Random as R

-- | Calculates an approximate value of pi.
estimatePi :: IO Double
estimatePi = do
    gen <- R.newStdGen
    let xs = R.randomRs (0::Double,1) gen
        points = take 1000000 $ zip xs (drop 1 xs)
    let pointsInCirc = filter (< 1) ((\(x,y) -> x^2 + y^2) <$> points)
    return $
        4 * (fromIntegral (length pointsInCirc) / fromIntegral (length points))