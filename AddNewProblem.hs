#!/usr/bin/env stack
-- stack --resolver lts-13.25 script

-- | Script to add boilerplate code to a new coding problem.
{-# LANGUAGE OverloadedStrings #-}

import Turtle
import qualified Control.Foldl as F
import qualified Data.Text as T
import Data.Char (isDigit)
import Data.Ord (comparing)
import System.Environment (getArgs)

solutionFileContents n =
    "module DailyCodingProblem.Problem" <> n <> ".Solution where\n\n"

specFileContents n = T.unlines [
      "module DailyCodingProblem.Problem" <> n <> "Spec (spec) where"
    , ""
    , "import Test.Hspec"
    , "import Test.Hspec.QuickCheck"
    , "import DailyCodingProblem.Problem" <> n <> ".Solution"
    , ""
    , "spec :: Spec"
    , "spec = undefined"
    ]

problemFolder n = "src/DailyCodingProblem" <> "/Problem" <> n

readmeContents n =
    "* [Problem " <> n <> "](" <> problemFolder n <> ")"

getProblemNumber = do
    let namesShell = toText . filename <$> ls "src/DailyCodingProblem"
    names' <- fold namesShell F.list

    case sequence names' of
        Left _ -> return 1
        Right names -> do
            let numsStr = filter (not . T.null) (T.takeWhileEnd isDigit <$> names)
                solvedProblems = read . T.unpack <$> numsStr
                prevProblem = maximum solvedProblems
            return (prevProblem+1)

createSrcFiles n = do
    let folderPath = fromText (problemFolder n)
    mkdir folderPath
    touch $ folderPath </> "README.md"
    touch $ folderPath </> "Solution.hs"
    writeTextFile (folderPath </> "Solution.hs") (solutionFileContents n)

createTestFiles n = do
    let testFileName = fromText ("test/DailyCodingProblem/Problem" <> n <> "Spec.hs")
    touch testFileName
    writeTextFile testFileName (specFileContents n)

addLinkToReadme n =
    append "README.md"
           (select $ textToLines $ readmeContents n)

buildProject = proc "stack" ["build", "--test", "--no-run-tests"] empty

main = do
    n <- T.pack . show <$> getProblemNumber
    createSrcFiles n
    createTestFiles n
    addLinkToReadme n
    buildProject