{-# LANGUAGE OverloadedStrings #-}

module Test.Chell.Main
	( defaultMain
	) where

import           Control.Monad (foldM, forM_, unless, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Char (ord)
import           Data.IORef (newIORef, readIORef, atomicModifyIORef)
import           Data.List (intercalate)
import qualified Data.Text
import           Data.Text (Text)
import qualified Data.Text.IO
import qualified System.Console.GetOpt as GetOpt
import           System.Environment (getArgs, getProgName)
import           System.Exit (exitSuccess, exitFailure)
import           System.IO (Handle, stderr, hPutStr, hPutStrLn)
import qualified System.IO as IO
import           System.Random (randomIO)
import           Text.Printf (printf)

import           Test.Chell.Types

data Option
	= OptionHelp
	| OptionVerbose
	| OptionXmlReport FilePath
	| OptionJsonReport FilePath
	| OptionLog FilePath
	| OptionSeed Int
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
	
	, GetOpt.Option [] ["seed"]
	  (GetOpt.ReqArg (\s -> case parseInt s of
	   	Just x -> OptionSeed x
	   	Nothing -> error ("Failed to parse --seed value " ++ show s)) "SEED")
	  "the seed used for random numbers in (for example) quickcheck"
	
	]

parseInt :: String -> Maybe Int
parseInt s = case [x | (x, "") <- reads s] of
	[x] -> Just x
	_ -> Nothing

getSeedOpt :: [Option] -> Maybe Int
getSeedOpt [] = Nothing
getSeedOpt ((OptionSeed s) : _) = Just s
getSeedOpt (_:os) = getSeedOpt os

usage :: String -> String
usage name = "Usage: " ++ name ++ " [OPTION...]"

-- | A simple default main function, which runs a list of tests and logs
-- statistics to stderr.
defaultMain :: [Suite] -> IO ()
defaultMain suites = do
	args <- getArgs
	let (options, filters, optionErrors) = GetOpt.getOpt GetOpt.Permute optionInfo args
	unless (null optionErrors) $ do
		name <- getProgName
		hPutStrLn stderr (concat optionErrors)
		hPutStrLn stderr (GetOpt.usageInfo (usage name) optionInfo)
		exitFailure
	
	when (OptionHelp `elem` options) $ do
		name <- getProgName
		putStrLn (GetOpt.usageInfo (usage name) optionInfo)
		exitSuccess
	
	let allTests = concatMap suiteTests suites
	let tests = if null filters
		then allTests
		else filter (matchesFilter filters) allTests
	
	seed <- case getSeedOpt options of
		Just s -> return s
		Nothing -> randomIO
	
	let testOptions = TestOptions
		{ testOptionSeed = seed
		}
	
	allPassed <- withReports options $ do
		ReportsM (mapM_ reportStart)
		allPassed <- foldM (\good t -> do
			thisGood <- reportTest testOptions t
			return (good && thisGood)) True tests
		ReportsM (mapM_ reportFinish)
		return allPassed
	
	if allPassed
		then exitSuccess
		else exitFailure

matchesFilter :: [String] -> Test -> Bool
matchesFilter strFilters = check where
	filters = map Data.Text.pack strFilters
	check t = any (matchName (testName t)) filters
	matchName name f = f == name || Data.Text.isPrefixOf (Data.Text.append f ".") name

data Report = Report
	{ reportStart :: IO ()
	, reportStartTest :: Text -> IO ()
	, reportFinishTest :: Text -> TestResult -> IO ()
	, reportFinish :: IO ()
	}

jsonReport :: Handle -> IO Report
jsonReport h = do
	commaRef <- newIORef False
	let comma = do
		needComma <- atomicModifyIORef commaRef (\c -> (True, c))
		if needComma
			then hPutStr h ", "
			else hPutStr h "  "
	let putNotes notes = do
		hPutStr h ", \"notes\": [\n"
		hPutStr h (intercalate "\n, " (do
			(key, value) <- notes
			return (concat
				[ "{\"key\": \""
				, escapeJSON key
				, "\", \"value\": \""
				, escapeJSON value
				, "\"}\n"
				])))
		hPutStrLn h "]"
	return (Report
		{ reportStart = do
			hPutStrLn h "{\"test-runs\": [ "
		, reportStartTest = \name -> do
			comma
			hPutStr h "{\"test\": \""
			hPutStr h (escapeJSON name)
			hPutStr h "\", \"result\": \""
		, reportFinishTest = \_ result -> case result of
			TestPassed notes -> do
				hPutStr h "passed\""
				putNotes notes
				hPutStrLn h "}"
			TestSkipped -> do
				hPutStrLn h "skipped\"}"
			TestFailed notes fs -> do
				hPutStrLn h "failed\", \"failures\": ["
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
				hPutStr h "]"
				putNotes notes
				hPutStrLn h "}"
			TestAborted notes msg -> do
				hPutStr h "aborted\", \"abortion\": {\"message\": \""
				hPutStr h (escapeJSON msg)
				hPutStr h "\"}"
				putNotes notes
				hPutStrLn h "}"
		, reportFinish = do
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
	{ reportStart = do
		hPutStrLn h "<?xml version=\"1.0\" encoding=\"utf8\"?>"
		hPutStrLn h "<report xmlns='urn:john-millikin:chell:report:1'>"
	, reportStartTest = \name -> do
		hPutStr h "\t<test-run test='"
		hPutStr h (escapeXML name)
		hPutStr h "' result='"
	, reportFinishTest = \_ result -> case result of
		TestPassed notes -> do
			hPutStrLn h "passed'>"
			putNotes notes
			hPutStrLn h "\t</test-run>"
		TestSkipped -> do
			hPutStrLn h "skipped'/>"
		TestFailed notes fs -> do
			hPutStrLn h "failed'>"
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
			putNotes notes
			hPutStrLn h "\t</test-run>"
		TestAborted notes msg -> do
			hPutStrLn h "aborted'>"
			hPutStr h "\t\t<abortion message='"
			hPutStr h (escapeXML msg)
			hPutStrLn h "'/>"
			putNotes notes
			hPutStrLn h "\t</test-run>"
	, reportFinish = do
		hPutStrLn h "</report>"
	} where
		putNotes notes = forM_ notes $ \(key, value) -> do
			hPutStr h "\t\t<note key=\""
			hPutStr h (escapeXML key)
			hPutStr h "\" value=\""
			hPutStr h (escapeXML value)
			hPutStrLn h "\"/>"

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
	
	let putNotes notes = forM_ notes $ \(key, value) -> do
		Data.Text.IO.hPutStr h key
		hPutStr h "="
		Data.Text.IO.hPutStrLn h value
	
	return (Report
		{ reportStart = return ()
		, reportStartTest = \_ -> return ()
		, reportFinishTest = \name result -> case result of
			TestPassed notes -> do
				when verbose $ do
					hPutStrLn h (replicate 70 '=')
					hPutStr h "PASSED: "
					Data.Text.IO.hPutStrLn h name
					putNotes notes
					hPutStr h "\n"
				incRef countPassed
			TestSkipped -> do
				when verbose $ do
					hPutStrLn h (replicate 70 '=')
					hPutStr h "SKIPPED: "
					Data.Text.IO.hPutStrLn h name
					hPutStr h "\n"
				incRef countSkipped
			TestFailed notes fs -> do
				hPutStrLn h (replicate 70 '=')
				hPutStr h "FAILED: "
				Data.Text.IO.hPutStrLn h name
				putNotes notes
				hPutStrLn h (replicate 70 '-')
				forM_ fs $ \(Failure loc msg) -> do
					case loc of
						Just loc' -> do
							Data.Text.IO.hPutStr h (locationFile loc')
							hPutStr h ":"
							hPutStrLn h (show (locationLine loc'))
						Nothing -> return ()
					Data.Text.IO.hPutStrLn h msg
					hPutStr h "\n"
				incRef countFailed
			TestAborted notes msg -> do
				hPutStrLn h (replicate 70 '=')
				hPutStr h "ABORTED: "
				Data.Text.IO.hPutStrLn h name
				putNotes notes
				hPutStrLn h (replicate 70 '-')
				Data.Text.IO.hPutStrLn h msg
				hPutStr h "\n"
				incRef countAborted
		, reportFinish = do
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

reportTest :: TestOptions -> Test -> ReportsM Bool
reportTest options t = do
	let name = testName t
	let notify io = ReportsM (mapM_ io)
	
	notify (\r -> reportStartTest r name)
	result <- liftIO (runTest t options)
	notify (\r -> reportFinishTest r name result)
	return $ case result of
		TestPassed{} -> True
		TestSkipped{} -> True
		TestFailed{} -> False
		TestAborted{} -> False
