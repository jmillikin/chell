{-# LANGUAGE CPP #-}

module Test.Chell.Output
	( Output(..)
	, plainOutput
	, colorOutput
	) where

import           Control.Monad (forM_, unless, when)
import           Data.Text (Text)
import qualified Data.Text.IO

#ifdef MIN_VERSION_ansi_terminal
import qualified System.Console.ANSI as AnsiTerminal
#endif

import           Test.Chell.Types

data Output = Output
	{ outputStart :: Test -> IO ()
	, outputResult :: Test -> TestResult -> IO ()
	}

plainOutput :: Bool -> Output
plainOutput v = Output
	{ outputStart = plainOutputStart v
	, outputResult = plainOutputResult v
	}

plainOutputStart :: Bool -> Test -> IO ()
plainOutputStart v t = when v $ do
	putStr "[ RUN   ] "
	Data.Text.IO.putStrLn (testName t)

plainOutputResult :: Bool -> Test -> TestResult -> IO ()
plainOutputResult v t (TestPassed _) = when v $ do
	putStr "[ PASS  ] "
	Data.Text.IO.putStrLn (testName t)
	putStrLn ""
plainOutputResult v t TestSkipped = when v $ do
	putStr "[ SKIP  ] "
	Data.Text.IO.putStrLn (testName t)
	putStrLn ""
plainOutputResult _ t (TestFailed notes fs) = do
	putStr "[ FAIL  ] "
	Data.Text.IO.putStrLn (testName t)
	printNotes notes
	printFailures fs
plainOutputResult _ t (TestAborted notes msg) = do
	putStr "[ ABORT ] "
	Data.Text.IO.putStrLn (testName t)
	printNotes notes
	putStr "  "
	Data.Text.IO.putStr msg
	putStrLn "\n"

colorOutput :: Bool -> Output
#ifndef MIN_VERSION_ansi_terminal
colorOutput = plainOutput
#else
colorOutput v = Output
	{ outputStart = colorOutputStart v
	, outputResult = colorOutputResult v
	}

colorOutputStart :: Bool -> Test -> IO ()
colorOutputStart v t = when v $ do
	putStr "[ RUN   ] "
	Data.Text.IO.putStrLn (testName t)

colorOutputResult :: Bool -> Test -> TestResult -> IO ()
colorOutputResult v t (TestPassed _) = when v $ do
	putStr "[ "
	AnsiTerminal.setSGR
		[ AnsiTerminal.SetColor AnsiTerminal.Foreground AnsiTerminal.Vivid AnsiTerminal.Green
		]
	putStr "PASS"
	AnsiTerminal.setSGR
		[ AnsiTerminal.Reset
		]
	putStr "  ] "
	Data.Text.IO.putStrLn (testName t)
	putStrLn ""
colorOutputResult v t TestSkipped = when v $ do
	putStr "[ "
	AnsiTerminal.setSGR
		[ AnsiTerminal.SetColor AnsiTerminal.Foreground AnsiTerminal.Vivid AnsiTerminal.Yellow
		]
	putStr "SKIP"
	AnsiTerminal.setSGR
		[ AnsiTerminal.Reset
		]
	putStr "  ] "
	Data.Text.IO.putStrLn (testName t)
	putStrLn ""
colorOutputResult _ t (TestFailed notes fs) = do
	putStr "[ "
	AnsiTerminal.setSGR
		[ AnsiTerminal.SetColor AnsiTerminal.Foreground AnsiTerminal.Vivid AnsiTerminal.Red
		]
	putStr "FAIL"
	AnsiTerminal.setSGR
		[ AnsiTerminal.Reset
		]
	putStr "  ] "
	Data.Text.IO.putStrLn (testName t)
	printNotes notes
	printFailures fs
colorOutputResult _ t (TestAborted notes msg) = do
	putStr "[ "
	AnsiTerminal.setSGR
		[ AnsiTerminal.SetColor AnsiTerminal.Foreground AnsiTerminal.Vivid AnsiTerminal.Red
		]
	putStr "ABORT"
	AnsiTerminal.setSGR
		[ AnsiTerminal.Reset
		]
	putStr " ] "
	Data.Text.IO.putStrLn (testName t)
	printNotes notes
	putStr "  "
	Data.Text.IO.putStr msg
	putStrLn "\n"
#endif

printNotes :: [(Text, Text)] -> IO ()
printNotes notes = unless (null notes) $ do
	forM_ notes $ \(key, value) -> do
		putStr "  note: "
		Data.Text.IO.putStr key
		putStr "="
		Data.Text.IO.putStrLn value
	putStrLn ""

printFailures :: [Failure] -> IO ()
printFailures fs = forM_ fs $ \(Failure loc msg) -> do
	putStr "  "
	case loc of
		Just loc' -> do
			Data.Text.IO.putStr (locationFile loc')
			putStr ":"
			putStrLn (show (locationLine loc'))
		Nothing -> return ()
	putStr "  "
	Data.Text.IO.putStr msg
	putStrLn "\n"
