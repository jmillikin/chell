{-# LANGUAGE OverloadedStrings #-}

module Test.Chell.Types where

import qualified Data.Text
import           Data.Text (Text)

data Test = Test Text (TestOptions -> IO TestResult)

data TestOptions = TestOptions
	{ testOptionSeed :: Int
	}

testName :: Test -> Text
testName (Test name _) = name

runTest :: Test -> TestOptions -> IO TestResult
runTest (Test _ io) = io

data TestResult
	= TestPassed [(Text, Text)]
	| TestSkipped
	| TestFailed [(Text, Text)] [Failure]
	| TestAborted [(Text, Text)] Text

data Failure = Failure (Maybe Location) Text

data Location = Location
	{ locationFile :: Text
	, locationModule :: Text
	, locationLine :: Integer
	}

-- | Running a 'Test' requires it to be contained in a 'Suite'. This gives
-- the test a name, so users know which test failed.
data Suite = Suite Text [Suite]
           | SuiteTest Test

test :: Test -> Suite
test = SuiteTest

suite :: Text -> [Suite] -> Suite
suite = Suite

suiteName :: Suite -> Text
suiteName (Suite name _) = name
suiteName (SuiteTest t) = testName t

-- | The full list of 'Test's contained within this 'Suite'. Each 'Test'
-- is returned with its name modified to include the name of its parent
-- 'Suite's.
suiteTests :: Suite -> [Test]
suiteTests = loop "" where
	loop prefix s = let
		name = if Data.Text.null prefix
			then suiteName s
			else Data.Text.concat [prefix, ".", suiteName s]
		in case s of
			Suite _ suites -> concatMap (loop name) suites
			SuiteTest (Test _ io) -> [Test name io]
