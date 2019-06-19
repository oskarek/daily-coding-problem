module DailyCodingProblem.Problem10.Solution where

import Control.Concurrent
import Control.Monad

-- | Schedule an IO action after a specified number of milliseconds.
schedule :: IO () -> Int -> IO ()
schedule action n = void $ forkIO (threadDelay (1000*n) >> action)
