{-# LANGUAGE OverloadedStrings #-}

module Test.Chell.Types where

import qualified Control.Exception
import           Control.Exception (SomeException, Handler(..), catches, throwIO)
import qualified Data.Text
import           Data.Text (Text)
import           System.Timeout (timeout)

data Test = Test Text (TestOptions -> IO TestResult)

instance Show Test where
	show (Test name _) = "<Test " ++ show name ++ ">"

data TestOptions = TestOptions
	{
	
	-- | Get the RNG seed for this test run. The seed is generated once, in
	-- 'defaultMain', and used for all tests. It is also logged to reports
	-- using a note.
	--
	-- When using 'defaultMain', users may specify a seed using the
	-- @--seed@ command-line option.
	  testOptionSeed :: Int
	
	-- | An optional timeout, in millseconds. Tests which run longer than
	-- this timeout will be aborted.
	--
	-- When using 'defaultMain', users may specify a timeout using the
	-- @--timeout@ command-line option.
	, testOptionTimeout :: Maybe Int
	}
	deriving (Show, Eq)

-- | Default test options.
--
-- @
--'testOptionSeed' defaultTestOptions = 0
--'testOptionTimeout' defaultTestOptions = Nothing
-- @
defaultTestOptions :: TestOptions
defaultTestOptions = TestOptions
	{ testOptionSeed = 0
	, testOptionTimeout = Nothing
	}

-- | @testName (Test name _) = name@
testName :: Test -> Text
testName (Test name _) = name

-- | Run a test, wrapped in error handlers. This will return 'TestAborted' if
-- the test throws an exception or times out.
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
	
	tried <- withTimeout (try getResult)
	case tried of
		Just (Right ret) -> return ret
		Nothing -> do
			notes <- getNotes
			return (TestAborted notes hitTimeout)
		Just (Left err) -> do
			notes <- getNotes
			return (TestAborted notes (Data.Text.pack err))

try :: IO a -> IO (Either String a)
try io = catches (fmap Right io) [Handler handleAsync, Handler handleExc] where
	handleAsync :: Control.Exception.AsyncException -> IO a
	handleAsync = throwIO
	
	handleExc :: SomeException -> IO (Either String a)
	handleExc exc = return (Left ("Test aborted due to exception: " ++ show exc))

data TestResult
	= TestPassed [(Text, Text)]
	| TestSkipped
	| TestFailed [(Text, Text)] [Failure]
	| TestAborted [(Text, Text)] Text
	deriving (Show, Eq)

data Failure = Failure (Maybe Location) Text
	deriving (Show, Eq)

data Location = Location
	{ locationFile :: Text
	, locationModule :: Text
	, locationLine :: Integer
	}
	deriving (Show, Eq)

-- | A tree of 'Test's; use the 'test' and 'suite' helper functions to build
-- up a 'Suite'.
data Suite = Suite Text [Suite]
           | SuiteTest Test

instance Show Suite where
	show s = "<Suite " ++ show (suiteName s) ++ ">"

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
