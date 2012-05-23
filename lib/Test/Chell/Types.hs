module Test.Chell.Types
	( Test
	, test
	, testName
	
	, TestOptions
	, defaultTestOptions
	, testOptionSeed
	, testOptionTimeout
	
	, TestResult(..) -- (..) is temporary
	, testPassed
	, testSkipped
	, testFailed
	, testAborted
	
	, Failure
	, failure
	, failureLocation
	, failureMessage
	
	, Location
	, location
	, locationFile
	, locationModule
	, locationLine
	
	, Suite
	, suite
	, suiteName
	, suiteTests
	
	, BuildSuite
	, SuiteOrTest(..)
	
	, runTest
	
	, handleJankyIO
	) where

import qualified Control.Exception
import           Control.Exception (SomeException, Handler(..), catches, throwIO)
import           System.Timeout (timeout)

data Test = Test String (TestOptions -> IO TestResult)

instance Show Test where
	show t = "<Test " ++ show (testName t) ++ ">"

-- | TODO
test :: String -> (TestOptions -> IO TestResult) -> Test
test = Test

-- | TODO
testName :: Test -> String
testName (Test name _) = name

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

data TestResult
	= TestPassed [(String, String)]
	| TestSkipped
	| TestFailed [(String, String)] [Failure]
	| TestAborted [(String, String)] String
	deriving (Show, Eq)

-- TODO: notes

-- | TODO
testPassed :: TestResult
testPassed = TestPassed []

testSkipped :: TestResult
testSkipped = TestSkipped

testFailed :: [Failure] -> TestResult
testFailed = TestFailed []

testAborted :: String -> TestResult
testAborted = TestAborted []

data Failure = Failure
	{ failureLocation :: Maybe Location
	, failureMessage :: String
	}
	deriving (Show, Eq)

failure :: Failure
failure = Failure Nothing ""

data Location = Location
	{ locationFile :: String
	, locationModule :: String
	, locationLine :: Integer
	}
	deriving (Show, Eq)

location :: Location
location = Location "" "" 0

-- | TODO
data Suite = Suite String [SOT]
data SOT
	= SOT_Suite Suite
	| SOT_Test Test

instance Show Suite where
	show s = "<Suite " ++ show (suiteName s) ++ ">"

class BuildSuite a where
	addChildren :: Suite -> a

instance BuildSuite Suite where
	addChildren (Suite name cs) = Suite name (reverse cs)

class SuiteOrTest a where
	toSOT :: a -> SOT
	skipIf_ :: Bool -> a -> a
	skipWhen_ :: IO Bool -> a -> a

instance SuiteOrTest Suite where
	toSOT = SOT_Suite
	skipIf_ skip s@(Suite name children) = if skip
		then Suite name (map skipSOT children)
		else s
	skipWhen_ p (Suite name children) = Suite name (map (skipWhenSOT p) children)

instance SuiteOrTest Test where
	toSOT = SOT_Test
	skipIf_ skip t@(Test name _) = if skip
		then Test name (\_ -> return TestSkipped)
		else t
	skipWhen_ p (Test name io) = Test name (\opts -> do
		skip <- p
		if skip then return TestSkipped else io opts)

skipSOT :: SOT -> SOT
skipSOT (SOT_Test (Test name _)) = SOT_Test (Test name (\_ -> return TestSkipped))
skipSOT (SOT_Suite (Suite name cs)) = SOT_Suite (Suite name (map skipSOT cs))

skipWhenSOT :: IO Bool -> SOT -> SOT
skipWhenSOT p (SOT_Suite s) = SOT_Suite (skipWhen_ p s)
skipWhenSOT p (SOT_Test t) = SOT_Test (skipWhen_ p t)

instance (SuiteOrTest t, BuildSuite s) => BuildSuite (t -> s) where
	addChildren (Suite name cs) = \b -> addChildren (Suite name (toSOT b : cs))

suite :: BuildSuite a => String -> a
suite name = addChildren (Suite name [])

suiteName :: Suite -> String
suiteName (Suite name _) = name

-- | The full list of 'Test's contained within this 'Suite'. Each 'Test'
-- is returned with its name modified to include the name of its parent
-- 'Suite's.
suiteTests :: Suite -> [Test]
suiteTests = go "" where
	prefixed prefix str = if null prefix
		then str
		else prefix ++ "." ++ str
	
	go prefix (Suite name children) = concatMap (step (prefixed prefix name)) children
	
	step prefix (SOT_Suite s) = go prefix s
	step prefix (SOT_Test (Test name io)) = [Test (prefixed prefix name) io]

-- | Run a test, wrapped in error handlers. This will return 'TestAborted' if
-- the test throws an exception or times out.
runTest :: Test -> TestOptions -> IO TestResult
runTest (Test _ io) options = handleJankyIO options (io options) (return [])

handleJankyIO :: TestOptions -> IO TestResult -> IO [(String, String)] -> IO TestResult
handleJankyIO opts getResult getNotes = do
	let withTimeout = case testOptionTimeout opts of
		Just time -> timeout (time * 1000)
		Nothing -> fmap Just
	
	let hitTimeout = str where
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
			return (TestAborted notes err)

try :: IO a -> IO (Either String a)
try io = catches (fmap Right io) [Handler handleAsync, Handler handleExc] where
	handleAsync :: Control.Exception.AsyncException -> IO a
	handleAsync = throwIO
	
	handleExc :: SomeException -> IO (Either String a)
	handleExc exc = return (Left ("Test aborted due to exception: " ++ show exc))
