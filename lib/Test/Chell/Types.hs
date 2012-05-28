module Test.Chell.Types
	( Test
	, test
	, testName
	
	, TestOptions
	, defaultTestOptions
	, testOptionSeed
	, testOptionTimeout
	
	, TestResult(TestPassed, TestSkipped, TestFailed, TestAborted)
	
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
	, SuiteOrTest
	, skipIf
	, skipWhen
	
	, runTest
	
	, handleJankyIO
	) where

import qualified Control.Exception
import           Control.Exception (SomeException, Handler(..), catches, throwIO)
import           System.Timeout (timeout)

-- | A 'Test' is, essentially, an IO action that returns a 'TestResult'. Tests
-- are aggregated into suites (see 'Suite').
data Test = Test String (TestOptions -> IO TestResult)

instance Show Test where
	showsPrec d (Test name _) = showParen (d > 10) (showString "Test " . shows name)

-- | Define a test, with the given name and implementation.
test :: String -> (TestOptions -> IO TestResult) -> Test
test = Test

-- | Get the name a test was given when it was defined; see 'test'.
testName :: Test -> String
testName (Test name _) = name

-- | Test options are passed to each test, and control details about how the
-- test should be run.
data TestOptions = TestOptions
	{
	
	-- | Get the RNG seed for this test run. The seed is generated once, in
	-- 'defaultMain', and used for all tests. It is also logged to reports
	-- using a note.
	--
	-- When using 'defaultMain', users may specify a seed using the
	-- @--seed@ command-line option.
	--
	-- 'testOptionSeed' is a field accessor, and can be used to update
	-- a 'TestOptions' value.
	  testOptionSeed :: Int
	
	-- | An optional timeout, in millseconds. Tests which run longer than
	-- this timeout will be aborted.
	--
	-- When using 'defaultMain', users may specify a timeout using the
	-- @--timeout@ command-line option.
	--
	-- 'testOptionTimeout' is a field accessor, and can be used to update
	-- a 'TestOptions' value.
	, testOptionTimeout :: Maybe Int
	}
	deriving (Show, Eq)

-- | Default test options.
--
-- >$ ghci
-- >Prelude> import Test.Chell
-- >
-- >Test.Chell> testOptionSeed defaultTestOptions
-- >0
-- >
-- >Test.Chell> testOptionTimeout defaultTestOptions
-- >Nothing
defaultTestOptions :: TestOptions
defaultTestOptions = TestOptions
	{ testOptionSeed = 0
	, testOptionTimeout = Nothing
	}

-- | The result of running a test.
--
-- To support future extensions to the testing API, any users of this module
-- who pattern-match against the 'TestResult' constructors should include a
-- default case. If no default case is provided, a warning will be issued.
data TestResult
	-- | The test passed, and generated the given notes.
	= TestPassed [(String, String)]
	
	-- | The test did not run, because it was skipped with 'skipIf'
	-- or 'skipWhen'.
	| TestSkipped
	
	-- | The test failed, generating the given notes and failures.
	| TestFailed [(String, String)] [Failure]
	
	-- | The test aborted with an error message, and generated the given
	-- notes.
	| TestAborted [(String, String)] String
	
	-- Not exported; used to generate GHC warnings for users who don't
	-- provide a default case.
	| TestResultCaseMustHaveDefault
	deriving (Show, Eq)

-- | Contains details about a test failure.
data Failure = Failure
	{
	-- | If given, the location of the failing assertion, expectation,
	-- etc.
	--
	-- 'failureLocation' is a field accessor, and can be used to update
	-- a 'Failure' value.
	  failureLocation :: Maybe Location
	
	-- | If given, a message which explains why the test failed.
	--
	-- 'failureMessage' is a field accessor, and can be used to update
	-- a 'Failure' value.
	, failureMessage :: String
	}
	deriving (Show, Eq)

-- | An empty 'Failure'; use the field accessors to populate this value.
failure :: Failure
failure = Failure Nothing ""

-- | Contains details about a location in the test source file.
data Location = Location
	{
	-- | A path to a source file, or empty if not provided.
	--
	-- 'locationFile' is a field accessor, and can be used to update
	-- a 'Location' value.
	  locationFile :: String
	
	-- | A Haskell module name, or empty if not provided.
	--
	-- 'locationModule' is a field accessor, and can be used to update
	-- a 'Location' value.
	, locationModule :: String
	
	-- | A line number, or Nothing if not provided.
	--
	-- 'locationLine' is a field accessor, and can be used to update
	-- a 'Location' value.
	, locationLine :: Maybe Integer
	}
	deriving (Show, Eq)

-- | An empty 'Location'; use the field accessors to populate this value.
location :: Location
location = Location "" "" Nothing

-- | A suite is a node in a hierarchy of tests, similar to a directory in the
-- filesystem. Each suite has a name and a list of children, which are either
-- suites or tests.
data Suite = Suite String [SOT]
	deriving (Show)

data SOT
	= SOT_Suite Suite
	| SOT_Test Test

instance Show SOT where
	showsPrec d (SOT_Test t) = showsPrec d t
	showsPrec d (SOT_Suite s) = showsPrec d s

-- | See 'suite'.
class BuildSuite a where
	addChildren :: Suite -> a

instance BuildSuite Suite where
	addChildren (Suite name cs) = Suite name (reverse cs)

-- | See 'suite'.
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

-- | Conditionally skip tests. Use this to avoid commenting out tests
-- which are currently broken, or do not work on the current platform.
--
-- @
--tests :: Suite
--tests = 'suite' \"tests\"
--    test_Foo
--    ('skipIf' builtOnUnix test_WindowsSpecific)
--    test_Bar
-- @
--
skipIf :: SuiteOrTest a => Bool -> a -> a
skipIf = skipIf_

-- | Conditionally skip tests, depending on the result of a runtime check. The
-- predicate is checked before each test is started.
--
-- @
--tests :: Suite
--tests = 'suite' \"tests\"
--    test_Foo
--    ('skipWhen' noNetwork test_PingGoogle)
--    test_Bar
-- @
skipWhen :: SuiteOrTest a => IO Bool -> a -> a
skipWhen = skipWhen_

instance (SuiteOrTest t, BuildSuite s) => BuildSuite (t -> s) where
	addChildren (Suite name cs) = \b -> addChildren (Suite name (toSOT b : cs))

-- | Define a new 'Suite', with the given name and children.
--
-- The type of this function allows any number of children to be added,
-- without requiring them to be homogenous types.
--
-- @
--test_Addition :: Test
--test_Subtraction :: Test
--test_Show :: Test
--
--tests_Math :: Suite
--tests_Math = 'suite' \"math\"
--    test_Addition
--    test_Subtraction
--
--tests_Prelude :: Suite
--tests_Prelude = 'suite' \"prelude\"
--    tests_Math
--    test_Show
-- @
--
-- Compatibility note: in GHC 7.0 and earlier, a maximum of 20 parameters may
-- be passed to variadic functions. Suites containing more than 20 children
-- may cause a compilation error that looks like:
--
-- >Context reduction stack overflow; size = 21
-- >Use -fcontext-stack=N to increase stack size to N
-- >  $dBuildSuite :: BuildSuite (Suite -> Test -> Test -> Suite)
--
-- There are three potential solutions:
--
-- 1. If possible, upgrade to a more recent version of GHC.
--
-- 2. Set the GHC flag @-fcontext-stack@ to a larger number.
--
-- 3. Re-organize your tests such that no suite has more than 20 children.
suite :: BuildSuite a => String -> a
suite name = addChildren (Suite name [])

-- | Get a suite's name. Suite names may be any string, but are typically
-- plain ASCII so users can easily type them on the command line.
--
-- >$ ghci chell-example.hs
-- >Ok, modules loaded: Main.
-- >
-- >*Main> suiteName tests_Math
-- >"math"
suiteName :: Suite -> String
suiteName (Suite name _) = name

-- | Get the full list of tests contained within this 'Suite'. Each test is
-- given its full name within the test hierarchy, where names are separated
-- by periods.
--
-- >$ ghci chell-example.hs
-- >Ok, modules loaded: Main.
-- >
-- >*Main> suiteTests tests_Math
-- >[Test "math.addition",Test "math.subtraction"]
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
