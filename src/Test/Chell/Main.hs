{-# LANGUAGE OverloadedStrings #-}

module Test.Chell.Main
	( defaultMain
	) where

import           Control.Monad (forM, forM_, unless, when)
import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.ByteString
import           Data.Char (ord)
import qualified Data.Text
import           Data.Text (Text, pack)
import qualified Data.Text.Encoding
import qualified Data.Text.IO
import qualified System.Console.GetOpt as GetOpt
import           System.Environment (getArgs, getProgName)
import           System.Exit (exitSuccess, exitFailure)
import qualified System.IO as IO
import           System.Random (randomIO)
import           Text.Printf (printf)

import           Test.Chell.Types

data Option
	= OptionHelp
	| OptionVerbose
	| OptionXmlReport FilePath
	| OptionJsonReport FilePath
	| OptionTextReport FilePath
	| OptionSeed Int
	| OptionTimeout Int
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
	
	, GetOpt.Option [] ["test-report"]
	  (GetOpt.ReqArg OptionTextReport "PATH")
	  "write a human-readable report"
	
	, GetOpt.Option [] ["seed"]
	  (GetOpt.ReqArg (\s -> case parseInt s of
	   	Just x -> OptionSeed x
	   	Nothing -> error ("Failed to parse --seed value " ++ show s)) "SEED")
	  "the seed used for random numbers in (for example) quickcheck"
	
	, GetOpt.Option [] ["timeout"]
	  (GetOpt.ReqArg (\s -> case parseTimeout s of
	  	Just x -> OptionTimeout x
	  	Nothing -> error ("Failed to parse --timeout value " ++ show s)) "TIMEOUT")
	  "the maximum duration of a test, in milliseconds"
	
	]

parseInt :: String -> Maybe Int
parseInt s = case [x | (x, "") <- reads s] of
	[x] -> Just x
	_ -> Nothing

parseTimeout :: String -> Maybe Int
parseTimeout s = do
	msec <- parseInt s
	if (toInteger msec) * 1000 > toInteger (maxBound :: Int)
		then Nothing
		else return msec

getSeedOpt :: [Option] -> Maybe Int
getSeedOpt [] = Nothing
getSeedOpt ((OptionSeed s) : _) = Just s
getSeedOpt (_:os) = getSeedOpt os

getTimeoutOpt :: [Option] -> Maybe Int
getTimeoutOpt [] = Nothing
getTimeoutOpt ((OptionTimeout s) : _) = Just s
getTimeoutOpt (_:os) = getTimeoutOpt os

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
		IO.hPutStrLn IO.stderr (concat optionErrors)
		IO.hPutStrLn IO.stderr (GetOpt.usageInfo (usage name) optionInfo)
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
		, testOptionTimeout = getTimeoutOpt options
		}
	
	let verbose = elem OptionVerbose options
	results <- forM tests $ \t -> do
		result <- runTest t testOptions
		printResult verbose t result
		return (t, result)
	
	let reports = getReports options
	forM_ reports $ \(path, fmt, toText) ->
		IO.withBinaryFile path IO.WriteMode $ \h -> do
			let text = toText results
			let bytes = Data.Text.Encoding.encodeUtf8 text
			when verbose $ do
				putStrLn ("Writing " ++ fmt ++ " report to " ++ show path)
			Data.ByteString.hPut h bytes
	
	let stats = resultStatistics results
	let (_, _, failed, aborted) = stats
	Data.Text.IO.putStrLn (formatResultStatistics stats)
	
	if failed == 0 && aborted == 0
		then exitSuccess
		else exitFailure

matchesFilter :: [String] -> Test -> Bool
matchesFilter strFilters = check where
	filters = map Data.Text.pack strFilters
	check t = any (matchName (testName t)) filters
	matchName name f = f == name || Data.Text.isPrefixOf (Data.Text.append f ".") name

printResult :: Bool -> Test -> TestResult -> IO ()
printResult verbose t result = case result of
	TestPassed _ -> when verbose $ do
		Data.Text.IO.putStr (testName t)
		putStrLn ": PASS"
	TestSkipped -> when verbose $ do
		Data.Text.IO.putStr (testName t)
		putStrLn ": SKIPPED"
	TestFailed notes fs -> do
		putStrLn (replicate 70 '=')
		Data.Text.IO.putStr (testName t)
		putStrLn ": FAILED"
		forM_ notes $ \(key, value) -> do
			putStr "\t"
			Data.Text.IO.putStr key
			putStr "="
			Data.Text.IO.putStrLn value
		putStrLn (replicate 70 '=')
		forM_ fs $ \(Failure loc msg) -> do
			case loc of
				Just loc' -> do
					Data.Text.IO.putStr (locationFile loc')
					putStr ":"
					putStrLn (show (locationLine loc'))
				Nothing -> return ()
			Data.Text.IO.putStr msg
			putStr "\n\n"
	TestAborted notes msg -> do
		putStrLn (replicate 70 '=')
		Data.Text.IO.putStr (testName t)
		putStrLn ": ABORTED"
		forM_ notes $ \(key, value) -> do
			putStr "\t"
			Data.Text.IO.putStr key
			putStr "="
			Data.Text.IO.putStrLn value
		putStrLn (replicate 70 '=')
		Data.Text.IO.putStr msg
		putStr "\n\n"

type Report = [(Test, TestResult)] -> Text

getReports :: [Option] -> [(FilePath, String, Report)]
getReports = concatMap step where
	step (OptionXmlReport path) = [(path, "XML", xmlReport)]
	step (OptionJsonReport path) = [(path, "JSON", jsonReport)]
	step (OptionTextReport path) = [(path, "text", textReport)]
	step _  = []

jsonReport :: [(Test, TestResult)] -> Text
jsonReport results = Writer.execWriter writer where
	tell = Writer.tell
	
	writer = do
		tell "{\"test-runs\": ["
		commas results tellResult
		tell "]}"
	
	tellResult (t, result) = case result of
		TestPassed notes -> do
			tell "{\"test\": \""
			tell (escapeJSON (testName t))
			tell "\", \"result\": \"passed\""
			tellNotes notes
			tell "}"
		TestSkipped -> do
			tell "{\"test\": \""
			tell (escapeJSON (testName t))
			tell "\", \"result\": \"skipped\"}"
		TestFailed notes fs -> do
			tell "{\"test\": \""
			tell (escapeJSON (testName t))
			tell "\", \"result\": \"failed\", \"failures\": ["
			commas fs $ \(Failure loc msg) -> do
				tell "{\"message\": \""
				tell (escapeJSON msg)
				tell "\""
				case loc of
					Just loc' -> do
						tell ", \"location\": {\"module\": \""
						tell (escapeJSON (locationModule loc'))
						tell "\", \"file\": \""
						tell (escapeJSON (locationFile loc'))
						tell "\", \"line\": "
						tell (pack (show (locationLine loc')))
						tell "}"
					Nothing -> return ()
				tell "}"
			tell "]"
			tellNotes notes
			tell "}"
		TestAborted notes msg -> do
			tell "{\"test\": \""
			tell (escapeJSON (testName t))
			tell "\", \"result\": \"aborted\", \"abortion\": {\"message\": \""
			tell (escapeJSON msg)
			tell "\"}"
			tellNotes notes
			tell "}"
	
	escapeJSON = Data.Text.concatMap (\c -> case c of
		'"' -> "\\\""
		'\\' -> "\\\\"
		_ | ord c <= 0x1F -> pack (printf "\\u%04X" (ord c))
		_ -> Data.Text.singleton c)
	
	tellNotes notes = do
		tell ", \"notes\": ["
		commas notes $ \(key, value) -> do
			tell "{\"key\": \""
			tell (escapeJSON key)
			tell "\", \"value\": \""
			tell (escapeJSON value)
			tell "\"}"
		tell "]"
	
	commas xs block = State.evalStateT (commaState xs block) False
	commaState xs block = forM_ xs $ \x -> do
		let tell' = lift . Writer.tell
		needComma <- State.get
		if needComma
			then tell' "\n, "
			else tell' "\n  "
		State.put True
		lift (block x)

xmlReport :: [(Test, TestResult)] -> Text
xmlReport results = Writer.execWriter writer where
	tell = Writer.tell
	
	writer = do
		tell "<?xml version=\"1.0\" encoding=\"utf8\"?>\n"
		tell "<report xmlns='urn:john-millikin:chell:report:1'>\n"
		mapM_ tellResult results
		tell "</report>"
	
	tellResult (t, result) = case result of
		TestPassed notes -> do
			tell "\t<test-run test='"
			tell (escapeXML (testName t))
			tell "' result='passed'>\n"
			tellNotes notes
			tell "\t</test-run>\n"
		TestSkipped -> do
			tell "\t<test-run test='"
			tell (escapeXML (testName t))
			tell "' result='skipped'/>\n"
		TestFailed notes fs -> do
			tell "\t<test-run test='"
			tell (escapeXML (testName t))
			tell "' result='failed'>\n"
			forM_ fs $ \(Failure loc msg) -> do
				tell "\t\t<failure message='"
				tell (escapeXML msg)
				case loc of
					Just loc' -> do
						tell "'>\n"
						tell "\t\t\t<location module='"
						tell (escapeXML (locationModule loc'))
						tell "' file='"
						tell (escapeXML (locationFile loc'))
						tell "' line='"
						tell (pack (show (locationLine loc')))
						tell "'/>\n"
						tell "\t\t</failure>\n"
					Nothing -> tell "'/>\n"
			tellNotes notes
			tell "\t</test-run>\n"
		TestAborted notes msg -> do
			tell "\t<test-run test='"
			tell (escapeXML (testName t))
			tell "' result='aborted'>\n"
			tell "\t\t<abortion message='"
			tell (escapeXML msg)
			tell "'/>\n"
			tellNotes notes
			tell "\t</test-run>\n"
	
	escapeXML = Data.Text.concatMap (\c -> case c of
		'&' -> "&amp;"
		'<' -> "&lt;"
		'>' -> "&gt;"
		'"' -> "&quot;"
		'\'' -> "&apos;"
		_ -> Data.Text.singleton c)
	
	tellNotes notes = forM_ notes $ \(key, value) -> do
		tell "\t\t<note key=\""
		tell (escapeXML key)
		tell "\" value=\""
		tell (escapeXML value)
		tell "\"/>\n"

textReport :: [(Test, TestResult)] -> Text
textReport results = Writer.execWriter writer where
	tell = Writer.tell
	
	writer = do
		forM_ results tellResult
		let stats = resultStatistics results
		tell (formatResultStatistics stats)
	
	tellResult (t, result) = case result of
		TestPassed notes -> do
			tell (Data.Text.replicate 70 "=")
			tell "\n"
			tell "PASSED: "
			tell (testName t)
			tell "\n"
			tellNotes notes
			tell "\n\n"
		TestSkipped -> do
			tell (Data.Text.replicate 70 "=")
			tell "\n"
			tell "SKIPPED: "
			tell (testName t)
			tell "\n\n"
		TestFailed notes fs -> do
			tell (Data.Text.replicate 70 "=")
			tell "\n"
			tell "FAILED: "
			tell (testName t)
			tell "\n"
			tellNotes notes
			tell (Data.Text.replicate 70 "-")
			tell "\n"
			forM_ fs $ \(Failure loc msg) -> do
				case loc of
					Just loc' -> do
						tell (locationFile loc')
						tell ":"
						tell (pack (show (locationLine loc')))
						tell "\n"
					Nothing -> return ()
				tell msg
				tell "\n\n"
		TestAborted notes msg -> do
			tell (Data.Text.replicate 70 "=")
			tell "\n"
			tell "ABORTED: "
			tell (testName t)
			tell "\n"
			tellNotes notes
			tell (Data.Text.replicate 70 "-")
			tell "\n"
			tell msg
			tell "\n\n"
	
	tellNotes notes = forM_ notes $ \(key, value) -> do
		tell key
		tell "="
		tell value
		tell "\n"

formatResultStatistics :: (Integer, Integer, Integer, Integer) -> Text
formatResultStatistics stats = Writer.execWriter writer where
	writer = do
		let (passed, skipped, failed, aborted) = stats
		if failed == 0 && aborted == 0
			then Writer.tell "PASS: "
			else Writer.tell "FAIL: "
		let putNum comma n what = Writer.tell $ if n == 1
			then pack (comma ++ "1 test " ++ what)
			else pack (comma ++ show n ++ " tests " ++ what)
		
		let total = sum [passed, skipped, failed, aborted]
		putNum "" total "run"
		when (passed > 0) (putNum ", " passed "passed")
		when (skipped > 0) (putNum ", " skipped "skipped")
		when (failed > 0) (putNum ", " failed "failed")
		when (aborted > 0) (putNum ", " aborted "aborted")

resultStatistics :: [(Test, TestResult)] -> (Integer, Integer, Integer, Integer)
resultStatistics results = State.execState state (0, 0, 0, 0) where
	state = forM_ results $ \(_, result) -> case result of
		TestPassed{} ->  State.modify (\(p, s, f, a) -> (p+1, s, f, a))
		TestSkipped{} -> State.modify (\(p, s, f, a) -> (p, s+1, f, a))
		TestFailed{} ->  State.modify (\(p, s, f, a) -> (p, s, f+1, a))
		TestAborted{} -> State.modify (\(p, s, f, a) -> (p, s, f, a+1))
