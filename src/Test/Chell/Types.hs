{-# LANGUAGE OverloadedStrings #-}

module Test.Chell.Types where

import qualified Control.Exception
import           Control.Exception (SomeException)
import qualified Data.Text
import           Data.Text (Text)
import           System.Timeout (timeout)

data Test = Test Text (TestOptions -> IO TestResult)

data TestOptions = TestOptions
	{ testOptionSeed :: Int
	, testOptionTimeout :: Maybe Int
	}

testName :: Test -> Text
testName (Test name _) = name

runTest :: Test -> TestOptions -> IO TestResult
runTest (Test _ io) options = handleJankyIO options (io options) (return [])

handleJankyIO :: TestOptions -> IO TestResult -> IO [(Text, Text)] -> IO TestResult
handleJankyIO opts getResult getNotes = do
	let withTimeout = case testOptionTimeout opts of
		Just time -> timeout (time * 1000)
		Nothing -> fmap Just
	
	let hitTimeout = Data.Text.pack str where
		str = "Test timed out after " ++ show time ++ " milliseconds"
		Just time = testOptionTimeout opts
	
	let errorExc :: SomeException -> Text
	    errorExc exc = Data.Text.pack ("Test aborted due to exception: " ++ show exc)
	
	tried <- withTimeout (Control.Exception.try getResult)
	case tried of
		Just (Right ret) -> return ret
		Nothing -> do
			notes <- getNotes
			return (TestAborted notes hitTimeout)
		Just (Left exc) -> do
			notes <- getNotes
			return (TestAborted notes (errorExc exc))

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
