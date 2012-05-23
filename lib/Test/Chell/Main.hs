{-# LANGUAGE TemplateHaskell #-}

module Test.Chell.Main
	( defaultMain
	) where

import           Control.Monad (forM, forM_, when)
import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Writer as Writer
import           Data.Char (ord)
import           Data.List (isPrefixOf)
import           System.Exit (exitSuccess, exitFailure)
import           System.IO (hPutStr, hPutStrLn, hIsTerminalDevice, stderr, stdout, withBinaryFile, IOMode(..))
import           System.Random (randomIO)
import           Text.Printf (printf)

import qualified Filesystem.Path.CurrentOS as Path
import           Options

import           Test.Chell.Output
import           Test.Chell.Types

$(defineOptions "MainOptions" $ do
	option "optVerbose" (\o -> o
		{ optionShortFlags = ['v']
		, optionLongFlags = ["verbose"]
		, optionType = optionTypeBool
		, optionDefault = "false"
		, optionDescription = "Print more output"
		})
	
	pathOption "optXmlReport" "xml-report" Path.empty
		"Write a parsable report to a given path, in XML."
	pathOption "optJsonReport" "json-report" Path.empty
		"Write a parsable report to a given path, in JSON."
	pathOption "optTextReport" "text-report" Path.empty
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
	
	option "optColor" (\o -> o
		{ optionLongFlags = ["color"]
		, optionType = optionTypeEnum ''ColorMode
			[ ("always", ColorModeAlways)
			, ("never", ColorModeNever)
			, ("auto", ColorModeAuto)
			]
		, optionDefault = "auto"
		, optionDescription = "Whether to enable color ('always', 'auto', or 'never')."
		})
	)

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
				hPutStrLn stderr "Test.Chell.defaultMain: Ignoring --timeout because it is too large."
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
	
	-- output mode
	output <- case optColor opts of
		ColorModeNever -> return (plainOutput (optVerbose opts))
		ColorModeAlways -> return (colorOutput (optVerbose opts))
		ColorModeAuto -> do
			isTerm <- hIsTerminalDevice stdout
			return $ if isTerm
				then colorOutput (optVerbose opts)
				else plainOutput (optVerbose opts)
	
	-- run tests
	results <- forM tests $ \t -> do
		outputStart output t
		result <- runTest t testOptions
		outputResult output t result
		return (t, result)
	
	-- generate reports
	let reports = getReports opts
	forM_ reports $ \(path, fmt, toText) ->
		withBinaryFile path WriteMode $ \h -> do
			when (optVerbose opts) $ do
				putStrLn ("Writing " ++ fmt ++ " report to " ++ show path)
			hPutStr h (toText results)
	
	let stats = resultStatistics results
	let (_, _, failed, aborted) = stats
	putStrLn (formatResultStatistics stats)
	
	if failed == 0 && aborted == 0
		then exitSuccess
		else exitFailure

matchesFilter :: [String] -> Test -> Bool
matchesFilter filters = check where
	check t = any (matchName (testName t)) filters
	matchName name f = f == name || isPrefixOf (f ++ ".") name

type Report = [(Test, TestResult)] -> String

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

jsonReport :: [(Test, TestResult)] -> String
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
			commas fs $ \f -> do
				tell "{\"message\": \""
				tell (escapeJSON (failureMessage f))
				tell "\""
				case failureLocation f of
					Just loc' -> do
						tell ", \"location\": {\"module\": \""
						tell (escapeJSON (locationModule loc'))
						tell "\", \"file\": \""
						tell (escapeJSON (locationFile loc'))
						tell "\", \"line\": "
						tell (show (locationLine loc'))
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
	
	escapeJSON = concatMap (\c -> case c of
		'"' -> "\\\""
		'\\' -> "\\\\"
		_ | ord c <= 0x1F -> printf "\\u%04X" (ord c)
		_ -> [c])
	
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

xmlReport :: [(Test, TestResult)] -> String
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
			forM_ fs $ \f -> do
				tell "\t\t<failure message='"
				tell (escapeXML (failureMessage f))
				case failureLocation f of
					Just loc' -> do
						tell "'>\n"
						tell "\t\t\t<location module='"
						tell (escapeXML (locationModule loc'))
						tell "' file='"
						tell (escapeXML (locationFile loc'))
						tell "' line='"
						tell (show (locationLine loc'))
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
	
	escapeXML = concatMap (\c -> case c of
		'&' -> "&amp;"
		'<' -> "&lt;"
		'>' -> "&gt;"
		'"' -> "&quot;"
		'\'' -> "&apos;"
		_ -> [c])
	
	tellNotes notes = forM_ notes $ \(key, value) -> do
		tell "\t\t<note key=\""
		tell (escapeXML key)
		tell "\" value=\""
		tell (escapeXML value)
		tell "\"/>\n"

textReport :: [(Test, TestResult)] -> String
textReport results = Writer.execWriter writer where
	tell = Writer.tell
	
	writer = do
		forM_ results tellResult
		let stats = resultStatistics results
		tell (formatResultStatistics stats)
	
	tellResult (t, result) = case result of
		TestPassed notes -> do
			tell (replicate 70 '=')
			tell "\n"
			tell "PASSED: "
			tell (testName t)
			tell "\n"
			tellNotes notes
			tell "\n\n"
		TestSkipped -> do
			tell (replicate 70 '=')
			tell "\n"
			tell "SKIPPED: "
			tell (testName t)
			tell "\n\n"
		TestFailed notes fs -> do
			tell (replicate 70 '=')
			tell "\n"
			tell "FAILED: "
			tell (testName t)
			tell "\n"
			tellNotes notes
			tell (replicate 70 '-')
			tell "\n"
			forM_ fs $ \f -> do
				case failureLocation f of
					Just loc' -> do
						tell (locationFile loc')
						tell ":"
						tell (show (locationLine loc'))
						tell "\n"
					Nothing -> return ()
				tell (failureMessage f)
				tell "\n\n"
		TestAborted notes msg -> do
			tell (replicate 70 '=')
			tell "\n"
			tell "ABORTED: "
			tell (testName t)
			tell "\n"
			tellNotes notes
			tell (replicate 70 '-')
			tell "\n"
			tell msg
			tell "\n\n"
	
	tellNotes notes = forM_ notes $ \(key, value) -> do
		tell key
		tell "="
		tell value
		tell "\n"

formatResultStatistics :: (Integer, Integer, Integer, Integer) -> String
formatResultStatistics stats = Writer.execWriter writer where
	writer = do
		let (passed, skipped, failed, aborted) = stats
		if failed == 0 && aborted == 0
			then Writer.tell "PASS: "
			else Writer.tell "FAIL: "
		let putNum comma n what = Writer.tell $ if n == 1
			then comma ++ "1 test " ++ what
			else comma ++ show n ++ " tests " ++ what
		
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
