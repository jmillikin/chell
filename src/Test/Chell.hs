{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Chell
	(
	
	-- * Main
	  defaultMain
	
	-- * Test suites
	, Suite
	, suite
	, suiteName
	, suiteTests
	, test
	, skipIf
	, skipWhen
	
	-- * Basic testing library
	-- $doc-basic-testing
	, Assertion (..)
	, AssertionResult (..)
	, IsAssertion
	, Assertions
	, assertions
	, assert
	, expect
	, Test.Chell.fail
	, trace
	, note
	
	-- ** Assertions
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
	, sameItems
	, equalItems
	, IsText
	, equalLines
	
	-- * Constructing tests
	, Test (..)
	, testName
	, runTest
	
	, TestOptions
	, testOptionSeed
	, testOptionTimeout
	, TestResult (..)
	, Failure (..)
	, Location (..)
	) where

import qualified Control.Applicative
import qualified Control.Exception
import           Control.Exception (Exception)
import           Control.Monad (ap, liftM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Algorithm.Patience as Patience
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy.Char8
import           Data.Foldable (Foldable, foldMap)
import           Data.List (foldl', intercalate, sort)
import           Data.Maybe (isJust, isNothing)
import           Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import qualified Data.Text
import           Data.Text (Text)
import qualified Data.Text.Lazy
import qualified Data.Text.IO

import qualified Language.Haskell.TH as TH

import           Test.Chell.Main (defaultMain)
import           Test.Chell.Types

-- | Get the RNG seed for this test run. The seed is generated once, in
-- 'defaultMain', and used for all tests. It is also logged to reports using
-- a note.
--
-- Users may specify a seed using the @--seed@ command-line option.
testOptionSeed :: TestOptions -> Int
testOptionSeed = testOptionSeed_

-- | An optional timeout, in millseconds. Tests which run longer than this
-- timeout will be aborted.
--
-- Users may specify a timeout using the @--timeout@ command-line option.
testOptionTimeout :: TestOptions -> Maybe Int
testOptionTimeout = testOptionTimeout_

-- | Conditionally skip a test. Use this to avoid commenting out tests
-- which are currently broken, or do not work on the current platform.
--
-- @
--tests = 'suite' \"tests\"
--    [ 'test' ('skipIf' builtOnUnix test_WindowsSpecific)
--    ]
-- @
--
skipIf :: Bool -> Test -> Test
skipIf skip t@(Test name _) = if skip
	then Test name (\_ -> return TestSkipped)
	else t

-- | Conditionally skip a test, depending on the result of a runtime check.
--
-- @
--tests = 'suite' \"tests\"
--    [ 'test' ('skipWhen' noNetwork test_PingGoogle)
--    ]
-- @
skipWhen :: IO Bool -> Test -> Test
skipWhen p (Test name io) = Test name $ \options -> do
	skipThis <- p
	if skipThis
		then return TestSkipped
		else io options

-- $doc-basic-testing
--
-- This library includes a few basic JUnit-style assertions, for use in
-- simple test suites where depending on a separate test framework is too
-- much trouble.

newtype Assertion = Assertion (IO AssertionResult)

data AssertionResult
	= AssertionPassed
	| AssertionFailed Text

class IsAssertion a where
	toAssertion :: a -> Assertion

instance IsAssertion Assertion where
	toAssertion = id

instance IsAssertion Bool where
	toAssertion x = Assertion (return (if x
		then AssertionPassed
		else AssertionFailed "$assert: boolean assertion failed"))

type TestState = (IORef [(Text, Text)], [Failure])
newtype Assertions a = Assertions { unAssertions :: TestState -> IO (Maybe a, TestState) }

instance Functor Assertions where
	fmap = liftM

instance Control.Applicative.Applicative Assertions where
	pure = return
	(<*>) = ap

instance Monad Assertions where
	return x = Assertions (\s -> return (Just x, s))
	m >>= f = Assertions (\s -> do
		(maybe_a, s') <- unAssertions m s
		case maybe_a of
			Nothing -> return (Nothing, s')
			Just a -> unAssertions (f a) s')

instance MonadIO Assertions where
	liftIO io = Assertions (\s -> do
		x <- io
		return (Just x, s))

-- | Convert a sequence of pass/fail assertions into a runnable test.
--
-- @
-- test_Equality :: Test
-- test_Equality = assertions \"equality\" $ do
--     $assert (1 == 1)
--     $assert (equal 1 1)
-- @
assertions :: Text -> Assertions a -> Test
assertions name testm = Test name io where
	io opts = do
		noteRef <- newIORef []
		
		let getNotes = fmap reverse (readIORef noteRef)
		
		let getResult = do
			res <- unAssertions testm (noteRef, [])
			case res of
				(_, (_, [])) -> do
					notes <- getNotes
					return (TestPassed notes)
				(_, (_, fs)) -> do
					notes <- getNotes
					return (TestFailed notes (reverse fs))
		
		handleJankyIO opts getResult getNotes

addFailure :: Maybe TH.Loc -> Bool -> Text -> Assertions ()
addFailure maybe_loc fatal msg = Assertions $ \(notes, fs) -> do
	let loc = do
		th_loc <- maybe_loc
		return $ Location
			{ locationFile = Data.Text.pack (TH.loc_filename th_loc)
			, locationModule = Data.Text.pack (TH.loc_module th_loc)
			, locationLine = toInteger (fst (TH.loc_start th_loc))
			}
	return ( if fatal then Nothing else Just ()
	       , (notes, Failure loc msg : fs))

-- | Cause a test to immediately fail, with a message.
--
-- 'fail' is a Template Haskell macro, to retain the source-file location
-- from which it was used. Its effective type is:
--
-- @
-- $fail :: 'Text' -> 'Assertions' ()
-- @
fail :: TH.Q TH.Exp -- :: Text -> Assertions ()
fail = do
	loc <- TH.location
	let qloc = liftLoc loc
	[| addFailure (Just $qloc) True |]

-- | Print a message from within a test. This is just a helper for debugging,
-- so you don't have to import @Debug.Trace@.
trace :: Text -> Assertions ()
trace msg = liftIO (Data.Text.IO.putStrLn msg)

-- | Attach metadata to a test run. This will be included in reports.
note :: Text -> Text -> Assertions ()
note key value = Assertions (\(notes, fs) -> do
	modifyIORef notes ((key, value) :)
	return (Just (), (notes, fs)))

liftLoc :: TH.Loc -> TH.Q TH.Exp
liftLoc loc = [| TH.Loc filename package module_ start end |] where
	filename = TH.loc_filename loc
	package = TH.loc_package loc
	module_ = TH.loc_module loc
	start = TH.loc_start loc
	end = TH.loc_end loc

assertAt :: IsAssertion assertion => TH.Loc -> Bool -> assertion -> Assertions ()
assertAt loc fatal assertion = do
	let Assertion io = toAssertion assertion
	result <- liftIO io
	case result of
		AssertionPassed -> return ()
		AssertionFailed err -> addFailure (Just loc) fatal err

-- | Run an 'Assertion'. If the assertion fails, the test will immediately
-- fail.
--
-- 'assert' is a Template Haskell macro, to retain the source-file location
-- from which it was used. Its effective type is:
--
-- @
-- $assert :: 'IsAssertion' assertion => assertion -> 'Assertions' ()
-- @
assert :: TH.Q TH.Exp -- :: IsAssertion assertion => assertion -> Assertions ()
assert = do
	loc <- TH.location
	let qloc = liftLoc loc
	[| assertAt $qloc True |]

-- | Run an 'Assertion'. If the assertion fails, the test will continue to
-- run until it finishes (or until an 'assert' fails).
--
-- 'expect' is a Template Haskell macro, to retain the source-file location
-- from which it was used. Its effective type is:
--
-- @
-- $expect :: 'IsAssertion' assertion => assertion -> 'Assertions' ()
-- @
expect :: TH.Q TH.Exp -- :: IsAssertion assertion => assertion -> Assertions ()
expect = do
	loc <- TH.location
	let qloc = liftLoc loc
	[| assertAt $qloc False |]

pure :: Bool -> String -> Assertion
pure True _ = Assertion (return AssertionPassed)
pure False err = Assertion (return (AssertionFailed (Data.Text.pack err)))

-- | Assert that two values are equal.
equal :: (Show a, Eq a) => a -> a -> Assertion
equal x y = pure (x == y) ("equal: " ++ show x ++ " is not equal to " ++ show y)

-- | Assert that two values are not equal.
notEqual :: (Eq a, Show a) => a -> a -> Assertion
notEqual x y = pure (x /= y) ("notEqual: " ++ show x ++ " is equal to " ++ show y)

-- | Assert that two values are within some delta of each other.
equalWithin :: (Real a, Show a) => a -> a
                                -> a -- ^ delta
                                -> Assertion
equalWithin x y delta = pure
	((x - delta <= y) && (x + delta >= y))
	("equalWithin: " ++ show x ++ " is not within " ++ show delta ++ " of " ++ show y)

-- | Assert that some value is @Just@.
just :: Maybe a -> Assertion
just x = pure (isJust x) ("just: received Nothing")

-- | Assert that some value is @Nothing@.
nothing :: Maybe a -> Assertion
nothing x = pure (isNothing x) ("nothing: received Just")

-- | Assert that some computation throws an exception matching the provided
-- predicate. This is mostly useful for exception types which do not have an
-- instance for @Eq@, such as @'Control.Exception.ErrorCall'@.
throws :: Exception err => (err -> Bool) -> IO a -> Assertion
throws p io = Assertion (do
	either_exc <- Control.Exception.try io
	return (case either_exc of
		Left exc -> if p exc
			then AssertionPassed
			else AssertionFailed (Data.Text.pack ("throws: exception " ++ show exc ++ " did not match predicate"))
		Right _ -> AssertionFailed (Data.Text.pack ("throws: no exception thrown"))))

-- | Assert that some computation throws an exception equal to the given
-- exception. This is better than just checking that the correct type was
-- thrown, because the test can also verify the exception contains the correct
-- information.
throwsEq :: (Eq err, Exception err, Show err) => err -> IO a -> Assertion
throwsEq expected io = Assertion (do
	either_exc <- Control.Exception.try io
	return (case either_exc of
		Left exc -> if exc == expected
			then AssertionPassed
			else AssertionFailed (Data.Text.pack ("throwsEq: exception " ++ show exc ++ " is not equal to " ++ show expected))
		Right _ -> AssertionFailed (Data.Text.pack ("throwsEq: no exception thrown"))))

-- | Assert a value is greater than another.
greater :: (Ord a, Show a) => a -> a -> Assertion
greater x y = pure (x > y) ("greater: " ++ show x ++ " is not greater than " ++ show y)

-- | Assert a value is greater than or equal to another.
greaterEqual :: (Ord a, Show a) => a -> a -> Assertion
greaterEqual x y = pure (x > y) ("greaterEqual: " ++ show x ++ " is not greater than or equal to " ++ show y)

-- | Assert a value is less than another.
lesser :: (Ord a, Show a) => a -> a -> Assertion
lesser x y = pure (x < y) ("lesser: " ++ show x ++ " is not less than " ++ show y)

-- | Assert a value is less than or equal to another.
lesserEqual :: (Ord a, Show a) => a -> a -> Assertion
lesserEqual x y = pure (x <= y) ("lesserEqual: " ++ show x ++ " is not less than or equal to " ++ show y)

-- | Assert that two containers have the same items, in any order.
sameItems :: (Foldable container, Show item, Ord item) => container item -> container item -> Assertion
sameItems x y = equalDiff' "sameItems" sort x y

-- | Assert that two containers have the same items, in the same order.
equalItems :: (Foldable container, Show item, Ord item) => container item -> container item -> Assertion
equalItems x y = equalDiff' "equalItems" id x y

equalDiff' :: (Foldable container, Show item, Ord item)
           => String
           -> ([item]
           -> [item])
           -> container item
           -> container item
           -> Assertion
equalDiff' label norm x y = checkDiff (items x) (items y) where
	items = norm . foldMap (:[])
	checkDiff xs ys = case checkItems (Patience.diff xs ys) of
		(same, diff) -> pure same diff
	
	checkItems diffItems = case foldl' checkItem (True, []) diffItems of
		(same, diff) -> (same, errorMsg (intercalate "\n" (reverse diff)))
	
	checkItem (same, acc) item = case item of
		Patience.Old t -> (False, ("\t- " ++ show t) : acc)
		Patience.New t -> (False, ("\t+ " ++ show t) : acc)
		Patience.Both t _-> (same, ("\t  " ++ show t) : acc)
	
	errorMsg diff = label ++ ": items differ\n" ++ diff

-- | Class for types which can be treated as text.
class IsText a where
	toLines :: a -> [a]
	unpack :: a -> String

instance IsText String where
	toLines = lines
	unpack = id

instance IsText Text where
	toLines = Data.Text.lines
	unpack = Data.Text.unpack

instance IsText Data.Text.Lazy.Text where
	toLines = Data.Text.Lazy.lines
	unpack = Data.Text.Lazy.unpack

-- | Uses @Data.ByteString.Char8@
instance IsText Data.ByteString.Char8.ByteString where
	toLines = Data.ByteString.Char8.lines
	unpack = Data.ByteString.Char8.unpack

-- | Uses @Data.ByteString.Lazy.Char8@
instance IsText Data.ByteString.Lazy.Char8.ByteString where
	toLines = Data.ByteString.Lazy.Char8.lines
	unpack = Data.ByteString.Lazy.Char8.unpack

-- | Assert that two pieces of text are equal. This uses a diff algorithm
-- to check line-by-line, so the error message will be easier to read on
-- large inputs.
equalLines :: (Ord a, IsText a) => a -> a -> Assertion
equalLines x y = checkDiff (toLines x) (toLines y) where
	checkDiff xs ys = case checkItems (Patience.diff xs ys) of
		(same, diff) -> pure same diff
	
	checkItems diffItems = case foldl' checkItem (True, []) diffItems of
		(same, diff) -> (same, errorMsg (intercalate "\n" (reverse diff)))
	
	checkItem (same, acc) item = case item of
		Patience.Old t -> (False, ("\t- " ++ unpack t) : acc)
		Patience.New t -> (False, ("\t+ " ++ unpack t) : acc)
		Patience.Both t _-> (same, ("\t  " ++ unpack t) : acc)
	
	errorMsg diff = "equalLines: lines differ\n" ++ diff
