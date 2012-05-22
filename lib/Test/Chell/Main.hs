{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Chell.Main
	( defaultMain
	) where

import           Control.Monad (forM, forM_, when)
import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.ByteString
import           Data.Char (ord)
import qualified Data.Text
import           Data.Text (Text, pack)
import qualified Data.Text.Encoding
import qualified Data.Text.IO
import           System.Exit (exitSuccess, exitFailure)
import qualified System.IO as IO
import           System.Random (randomIO)
import           Text.Printf (printf)

import qualified Filesystem.Path.CurrentOS as Path
import           Options

import           Test.Chell.Types

defineOptions "MainOptions" $ do
	option "optVerbose" (\o -> o
		{ optionShortFlags = ['v']
		, optionLongFlags = ["verbose"]
		, optionType = optionTypeBool
		, optionDefault = "false"
		, optionDescription = "Print more output"
		})
	
	pathOption "optXmlReport" "xml-report" ""
		"Write a parsable report to a given path, in XML."
	pathOption "optJsonReport" "json-report" ""
		"Write a parsable report to a given path, in JSON."
	pathOption "optTextReport" "text-report" ""
		"Write a human-readable report to a given path."
	
	option "optSeed" (\o -> o
		{ optionLongFlags = ["seed"]
		, optionType = optionTypeMaybe optionTypeInt
		, optionDefault = ""
		, optionDescription = "The seed used for random numbers in (for example) quickcheck."
		})
	
	option "optTimeout" (\o -> o
		{ optionLongFlags = ["timeout"]
		, optionType = optionTypeMaybe optionTypeInt
		, optionDefault = ""
		, optionDescription = "The maximum duration of a test, in milliseconds."
		})

-- | A simple default main function, which runs a list of tests and logs
-- statistics to stderr.
defaultMain :: [Suite] -> IO ()
defaultMain suites = runCommand $ \opts args -> do
	-- validate/sanitize test options
	seed <- case optSeed opts of
		Just s -> return s
		Nothing -> randomIO
	timeout <- case optTimeout opts of
		Nothing -> return Nothing
		Just t -> if toInteger t * 1000 > toInteger (maxBound :: Int)
			then do
				IO.hPutStrLn IO.stderr "Test.Chell.defaultMain: Ignoring --timeout because it is too large."
				return Nothing
			else return (Just t)
	let testOptions = defaultTestOptions
		{ testOptionSeed = seed
		, testOptionTimeout = timeout
		}
	
	-- find which tests to run
	let allTests = concatMap suiteTests suites
	let tests = if null args
		then allTests
		else filter (matchesFilter args) allTests
	
	-- run tests
	results <- forM tests $ \t -> do
		result <- runTest t testOptions
		printResult (optVerbose opts) t result
		return (t, result)
	
	-- generate reports
	let reports = getReports opts
	forM_ reports $ \(path, fmt, toText) ->
		IO.withBinaryFile path IO.WriteMode $ \h -> do
			let text = toText results
			let bytes = Data.Text.Encoding.encodeUtf8 text
			when (optVerbose opts) $ do
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

getReports :: MainOptions -> [(String, String, Report)]
getReports opts = concat [xml, json, text] where
	xml = case optXmlReport opts of
		p | Path.null p -> []
		p -> [(Path.encodeString p, "XML", xmlReport)]
	json = case optJsonReport opts of
		p | Path.null p -> []
		p -> [(Path.encodeString p, "JSON", jsonReport)]
	text = case optTextReport opts of
		p | Path.null p -> []
		p -> [(Path.encodeString p, "text", textReport)]

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
		(putNum ", " passed "passed")
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
