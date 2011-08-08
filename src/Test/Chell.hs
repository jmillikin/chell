{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Chell
	(
	
	-- * Tests
	  Test (..)
	, TestResult (..)
	, Failure (..)
	, Location (..)
	, skip
	, skipIf
	
	-- * Suites
	, Suite
	, suite
	, test
	, suiteName
	, suiteTests
	
	-- * Main
	, defaultMain
	
	-- * basic
	, Assertion
	, IsAssertion
	, Assertions
	, assertions
	, assert
	, expect
	, Test.Chell.fail
	, trace
	
	-- ** assertions
	, equal
	, notEqual
	, equalWithin
	, just
	, nothing
	, throws
	, throwsEq
	, greater
	, greaterEqual
	, lesser
	, lesserEqual
	) where

import qualified Control.Exception
import           Control.Exception (Exception)
import           Control.Monad (foldM, forM_, unless, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Char (ord)
import           Data.List (intercalate)
import           Data.Maybe (isJust, isNothing)
import           Data.IORef (newIORef, readIORef, atomicModifyIORef)
import qualified Data.Text
import           Data.Text (Text)
import qualified Data.Text.IO
import qualified System.Console.GetOpt as GetOpt
import           System.Environment (getArgs, getProgName)
import           System.Exit (exitSuccess, exitFailure)
import           System.IO (Handle, stderr, hPutStr, hPutStrLn)
import qualified System.IO as IO
import           Text.Printf (printf)

import qualified Language.Haskell.TH as TH

newtype Test = Test { runTest :: IO TestResult }

data TestResult
	= TestResultPassed
	| TestResultSkipped
	| TestResultFailed [Failure]
	| TestResultAborted Text

data Failure = Failure (Maybe Location) Text

data Location = Location
	{ locationFile :: Text
	, locationModule :: Text
	, locationLine :: Integer
	}

skip :: Test
skip = Test (return TestResultSkipped)

skipIf :: IO Bool -> Test -> Test
skipIf p t = Test $ do
	skipThis <- p
	if skipThis
		then return TestResultSkipped
		else runTest t

data Suite = Suite Text [Suite]
           | SuiteTest Text Test

test :: Text -> Test -> Suite
test = SuiteTest

suite :: Text -> [Suite] -> Suite
suite = Suite

suiteName :: Suite -> Text
suiteName (Suite name _) = name
suiteName (SuiteTest name _) = name

suiteTests :: Suite -> [(Text, Test)]
suiteTests = loop "" where
	loop prefix s = let
		name = if Data.Text.null prefix
			then suiteName s
			else Data.Text.concat [prefix, ".", suiteName s]
		in case s of
			Suite _ suites -> concatMap (loop name) suites
			SuiteTest _ t -> [(name, t)]

newtype Assertion = Assertion (IO (Maybe Text))

class IsAssertion a where
	toAssertion :: a -> Assertion

instance IsAssertion Assertion where
	toAssertion = id

instance IsAssertion Bool where
	toAssertion x = Assertion (return (if x
		then Nothing
		else Just "boolean assertion failed"))

type Assertions = TestM ()
newtype TestM a = TestM { unTestM :: [Failure] -> IO (Maybe a, [Failure]) }

instance Monad TestM where
	return x = TestM (\s -> return (Just x, s))
	m >>= f = TestM (\s -> do
		(maybe_a, s') <- unTestM m s
		case maybe_a of
			Nothing -> return (Nothing, s')
			Just a -> unTestM (f a) s')

instance MonadIO TestM where
	liftIO io = TestM (\s -> do
		x <- io
		return (Just x, s))

assertions :: Assertions -> Test
assertions testm = Test io where
	io = do
		tried <- Control.Exception.try (unTestM testm [])
		return $ case tried of
			Left exc -> TestResultAborted (errorExc exc)
			Right (_, []) -> TestResultPassed
			Right (_, fs) -> TestResultFailed fs
	
	errorExc :: Control.Exception.SomeException -> Text
	errorExc exc = Data.Text.pack ("Test aborted due to exception: " ++ show exc)

addFailure :: Maybe TH.Loc -> Bool -> Text -> Assertions
addFailure maybe_loc fatal msg = TestM $ \fs -> do
	let loc = do
		th_loc <- maybe_loc
		return $ Location
			{ locationFile = Data.Text.pack (TH.loc_filename th_loc)
			, locationModule = Data.Text.pack (TH.loc_module th_loc)
			, locationLine = toInteger (fst (TH.loc_start th_loc))
			}
	return ( if fatal then Nothing else Just ()
	       , Failure loc msg : fs)

fail :: TH.Q TH.Exp -- :: Text -> Assertions
fail = do
	loc <- TH.location
	let qloc = liftLoc loc
	[| addFailure (Just $qloc) True |]

trace :: Text -> Assertions
trace msg = liftIO (Data.Text.IO.putStrLn msg)

liftLoc :: TH.Loc -> TH.Q TH.Exp
liftLoc loc = [| TH.Loc filename package module_ start end |] where
	filename = TH.loc_filename loc
	package = TH.loc_package loc
	module_ = TH.loc_module loc
	start = TH.loc_start loc
	end = TH.loc_end loc

assertAt :: IsAssertion assertion => TH.Loc -> Bool -> assertion -> Assertions
assertAt loc fatal assertion = do
	let Assertion io = toAssertion assertion
	result <- liftIO io
	case result of
		Nothing -> return ()
		Just err -> addFailure (Just loc) fatal err

assert :: TH.Q TH.Exp -- :: IsAssertion assertion => assertion -> Assertions
assert = do
	loc <- TH.location
	let qloc = liftLoc loc
	[| assertAt $qloc True |]

expect :: TH.Q TH.Exp -- :: IsAssertion assertion => assertion -> Assertions
expect = do
	loc <- TH.location
	let qloc = liftLoc loc
	[| assertAt $qloc False |]

data Option
	= OptionHelp
	| OptionVerbose
	| OptionXmlReport FilePath
	| OptionJsonReport FilePath
	| OptionLog FilePath
	deriving (Show, Eq)

optionInfo :: [GetOpt.OptDescr Option]
optionInfo =
	[ GetOpt.Option ['h'] ["help"]
	  (GetOpt.NoArg OptionHelp)
	  "show this help"
	
	, GetOpt.Option ['v'] ["verbose"]
	  (GetOpt.NoArg OptionVerbose)
	  "print more output"
	
	, GetOpt.Option [] ["xml-report"]
	  (GetOpt.ReqArg OptionXmlReport "PATH")
	  "write a parsable report to a file, in XML"
	
	, GetOpt.Option [] ["json-report"]
	  (GetOpt.ReqArg OptionJsonReport "PATH")
	  "write a parsable report to a file, in JSON"
	
	, GetOpt.Option [] ["log"]
	  (GetOpt.ReqArg OptionLog "PATH")
	  "write a full log (always max verbosity) to a file path"
	]

usage :: String -> String
usage name = "Usage: " ++ name ++ " [OPTION...]"

defaultMain :: [Suite] -> IO ()
defaultMain suites = do
	args <- getArgs
	let (options, _, optionErrors) = GetOpt.getOpt GetOpt.Permute optionInfo args
	unless (null optionErrors) $ do
		name <- getProgName
		hPutStrLn stderr (concat optionErrors)
		hPutStrLn stderr (GetOpt.usageInfo (usage name) optionInfo)
		exitFailure
	
	when (OptionHelp `elem` options) $ do
		name <- getProgName
		putStrLn (GetOpt.usageInfo (usage name) optionInfo)
		exitSuccess
	
	let tests = concatMap suiteTests suites
	allPassed <- withReports options $ do
		ReportsM (mapM_ reportStarted)
		allPassed <- foldM (\good t -> do
			thisGood <- reportTest t
			return (good && thisGood)) True tests
		ReportsM (mapM_ reportFinished)
		return allPassed
	
	if allPassed
		then exitSuccess
		else exitFailure

data Report = Report
	{ reportStarted :: IO ()
	, reportPassed :: Text -> IO ()
	, reportSkipped :: Text -> IO ()
	, reportFailed :: Text -> [Failure] -> IO ()
	, reportAborted :: Text -> Text -> IO ()
	, reportFinished :: IO ()
	}

jsonReport :: Handle -> IO Report
jsonReport h = do
	commaRef <- newIORef False
	let comma = do
		needComma <- atomicModifyIORef commaRef (\c -> (True, c))
		if needComma
			then hPutStr h ", "
			else hPutStr h "  "
	return (Report
		{ reportStarted = do
			hPutStrLn h "{\"test-runs\": [ "
		, reportPassed = \name -> do
			comma
			hPutStr h "{\"test\": \""
			hPutStr h (escapeJSON name)
			hPutStrLn h "\", \"result\": \"passed\"}"
		, reportSkipped = \name -> do
			comma
			hPutStr h "{\"test\": \""
			hPutStr h (escapeJSON name)
			hPutStrLn h "\", \"result\": \"skipped\"}"
		, reportFailed = \name fs -> do
			comma
			hPutStr h "{\"test\": \""
			hPutStr h (escapeJSON name)
			hPutStrLn h "\", \"result\": \"failed\", \"failures\": ["
			hPutStrLn h (intercalate "\n, " (do
				Failure loc msg <- fs
				let locString = case loc of
					Just loc' -> concat
						[ ", \"location\": {\"module\": \""
						, escapeJSON (locationModule loc')
						, "\", \"file\": \""
						, escapeJSON (locationFile loc')
						, "\", \"line\": "
						, show (locationLine loc')
						, "}"
						]
					Nothing -> ""
				return ("{\"message\": \"" ++ escapeJSON msg ++ "\"" ++ locString ++ "}")))
			hPutStrLn h "]}"
		, reportAborted = \name msg -> do
			comma
			hPutStr h "{\"test\": \""
			hPutStr h (escapeJSON name)
			hPutStr h "\", \"result\": \"aborted\", \"abortion\": {\"message\": \""
			hPutStr h (escapeJSON msg)
			hPutStrLn h "\"}}"
		, reportFinished = do
			hPutStrLn h "]}"
		})

escapeJSON :: Text -> String
escapeJSON = concatMap (\c -> case c of
	'"' -> "\\\""
	'\\' -> "\\\\"
	_ | ord c <= 0x1F -> printf "\\u%04X" (ord c)
	_ -> [c]) . Data.Text.unpack

xmlReport :: Handle -> Report
xmlReport h = Report
	{ reportStarted = do
		hPutStrLn h "<?xml version=\"1.0\" encoding=\"utf8\"?>"
		hPutStrLn h "<report xmlns='urn:john-millikin:chell:report:1'>"
	, reportPassed = \name -> do
		hPutStr h "\t<test-run test='"
		hPutStr h (escapeXML name)
		hPutStrLn h "' result='passed'/>"
	, reportSkipped = \name -> do
		hPutStr h "\t<test-run test='"
		hPutStr h (escapeXML name)
		hPutStrLn h "' result='skipped'/>"
	, reportFailed = \name fs -> do
		hPutStr h "\t<test-run test='"
		hPutStr h (escapeXML name)
		hPutStrLn h "' result='failed'>"
		forM_ fs $ \(Failure loc msg) -> do
			hPutStr h "\t\t<failure message='"
			hPutStr h (escapeXML msg)
			case loc of
				Just loc' -> do
					hPutStrLn h "'>"
					hPutStr h "\t\t\t<location module='"
					hPutStr h (escapeXML (locationModule loc'))
					hPutStr h "' file='"
					hPutStr h (escapeXML (locationFile loc'))
					hPutStr h "' line='"
					hPutStr h (show (locationLine loc'))
					hPutStrLn h "'/>"
					hPutStrLn h "\t\t</failure>"
				Nothing -> hPutStrLn h "'/>"
		hPutStrLn h "\t</test-run>"
	, reportAborted = \name msg -> do
		hPutStr h "\t<test-run test='"
		hPutStr h (escapeXML name)
		hPutStrLn h "' result='aborted'>"
		hPutStr h "\t\t<abortion message='"
		hPutStr h (escapeXML msg)
		hPutStrLn h "'/>"
		hPutStrLn h "\t</test-run>"
	, reportFinished = do
		hPutStrLn h "</report>"
	}

escapeXML :: Text -> String
escapeXML = concatMap (\c -> case c of
	'&' -> "&amp;"
	'<' -> "&lt;"
	'>' -> "&gt;"
	'"' -> "&quot;"
	'\'' -> "&apos;"
	_ -> [c]) . Data.Text.unpack

textReport :: Bool -> Handle -> IO Report
textReport verbose h = do
	countPassed <- newIORef (0 :: Integer)
	countSkipped <- newIORef (0 :: Integer)
	countFailed <- newIORef (0 :: Integer)
	countAborted <- newIORef (0 :: Integer)
	
	let incRef ref = atomicModifyIORef ref (\a -> (a + 1, ()))
	
	return (Report
		{ reportStarted = return ()
		, reportPassed = \name -> do
			when verbose $ do
				hPutStr h "PASS "
				Data.Text.IO.hPutStrLn h name
			incRef countPassed
		, reportSkipped = \name -> do
			when verbose $ do
				hPutStr h "SKIP "
				Data.Text.IO.hPutStrLn h name
			incRef countSkipped
		, reportFailed = \name fs -> do
			hPutStr h "FAIL "
			Data.Text.IO.hPutStrLn h name
			forM_ fs $ \(Failure loc msg) -> do
				case loc of
					Just loc' -> do
						hPutStr h "\t"
						Data.Text.IO.hPutStr h (locationFile loc')
						hPutStr h ":"
						hPutStrLn h (show (locationLine loc'))
					Nothing -> return ()
				hPutStr h "\t\t"
				Data.Text.IO.hPutStrLn h msg
				hPutStr h "\n"
			incRef countFailed
		, reportAborted = \name msg -> do
			hPutStr h "ABRT "
			Data.Text.IO.hPutStrLn h name
			hPutStr h "\t"
			Data.Text.IO.hPutStrLn h msg
			hPutStr h "\n"
			incRef countAborted
		, reportFinished = do
			n_passed <- readIORef countPassed
			n_skipped <- readIORef countSkipped
			n_failed <- readIORef countFailed
			n_aborted <- readIORef countAborted
			if n_failed == 0 && n_aborted == 0
				then hPutStr h "PASS: "
				else hPutStr h "FAIL: "
			let putNum comma n what = hPutStr h $ if n == 1
				then comma ++ "1 test " ++ what
				else comma ++ show n ++ " tests " ++ what
			
			let total = sum [n_passed, n_skipped, n_failed, n_aborted]
			putNum "" total "run"
			when (n_passed > 0) (putNum ", " n_passed "passed")
			when (n_skipped > 0) (putNum ", " n_skipped "skipped")
			when (n_failed > 0) (putNum ", " n_failed "failed")
			when (n_aborted > 0) (putNum ", " n_aborted "aborted")
			hPutStr h "\n"
		})

withReports :: [Option] -> ReportsM a -> IO a
withReports opts reportsm = do
	let loop [] reports = runReportsM reportsm (reverse reports)
	    loop (o:os) reports = case o of
	    	OptionXmlReport path -> IO.withBinaryFile path IO.WriteMode
	    		(\h -> loop os (xmlReport h : reports))
	    	OptionJsonReport path -> IO.withBinaryFile path IO.WriteMode
	    		(\h -> jsonReport h >>= \r -> loop os (r : reports))
	    	OptionLog path -> IO.withBinaryFile path IO.WriteMode
	    		(\h -> textReport True h >>= \r -> loop os (r : reports))
	    	_ -> loop os reports
	
	console <- textReport (OptionVerbose `elem` opts) stderr
	loop opts [console]

newtype ReportsM a = ReportsM { runReportsM :: [Report] -> IO a }

instance Monad ReportsM where
	return x = ReportsM (\_ -> return x)
	m >>= f = ReportsM (\reports -> do
		x <- runReportsM m reports
		runReportsM (f x) reports)

instance MonadIO ReportsM where
	liftIO io = ReportsM (\_ -> io)

reportTest :: (Text, Test) -> ReportsM Bool
reportTest (name, t) = do
	result <- liftIO (runTest t)
	case result of
		TestResultPassed -> do
			passed name
			return True
		TestResultSkipped -> do
			skipped name
			return True
		TestResultFailed fs -> do
			failed name fs
			return False
		TestResultAborted msg -> do
			aborted name msg
			return False

passed :: Text -> ReportsM ()
passed name = ReportsM (mapM_ (\r -> reportPassed r name))

skipped :: Text -> ReportsM ()
skipped name = ReportsM (mapM_ (\r -> reportSkipped r name))

failed :: Text -> [Failure] -> ReportsM ()
failed name fs = ReportsM (mapM_ (\r -> reportFailed r name fs))

aborted :: Text -> Text -> ReportsM ()
aborted name msg = ReportsM (mapM_ (\r -> reportAborted r name msg))

pure :: Bool -> String -> Assertion
pure True _ = Assertion (return Nothing)
pure False err = Assertion (return (Just (Data.Text.pack err)))

equal :: (Show a, Eq a) => a -> a -> Assertion
equal x y = pure (x == y) ("equal: " ++ show x ++ " is not equal to " ++ show y)

notEqual :: (Eq a, Show a) => a -> a -> Assertion
notEqual x y = pure (x /= y) ("notEqual: " ++ show x ++ " is equal to " ++ show y)

equalWithin :: (Real a, Show a) => a -> a -> a -> Assertion
equalWithin x y delta = pure
	((x - delta <= y) && (x + delta >= y))
	("equalWithin: " ++ show x ++ " is not within " ++ show delta ++ " of " ++ show y)

just :: Maybe a -> Assertion
just x = pure (isJust x) ("just: received Nothing")

nothing :: Maybe a -> Assertion
nothing x = pure (isNothing x) ("nothing: received Just")

throws :: Exception err => (err -> Bool) -> IO a -> Assertion
throws p io = Assertion (do
	either_exc <- Control.Exception.try io
	return (case either_exc of
		Left exc -> if p exc
			then Nothing
			else Just (Data.Text.pack ("throws: exception " ++ show exc ++ " did not match predicate"))
		Right _ -> Just (Data.Text.pack ("throws: no exception thrown"))))

throwsEq :: (Eq err, Exception err, Show err) => err -> IO a -> Assertion
throwsEq expected io = Assertion (do
	either_exc <- Control.Exception.try io
	return (case either_exc of
		Left exc -> if exc == expected
			then Nothing
			else Just (Data.Text.pack ("throwsEq: exception " ++ show exc ++ " is not equal to " ++ show expected))
		Right _ -> Nothing))

greater :: (Ord a, Show a) => a -> a -> Assertion
greater x y = pure (x > y) ("greater: " ++ show x ++ " is not greater than " ++ show y)

greaterEqual :: (Ord a, Show a) => a -> a -> Assertion
greaterEqual x y = pure (x > y) ("greaterEqual: " ++ show x ++ " is not greater than or equal to " ++ show y)

lesser :: (Ord a, Show a) => a -> a -> Assertion
lesser x y = pure (x < y) ("lesser: " ++ show x ++ " is not less than " ++ show y)

lesserEqual :: (Ord a, Show a) => a -> a -> Assertion
lesserEqual x y = pure (x <= y) ("lesserEqual: " ++ show x ++ " is not less than or equal to " ++ show y)
