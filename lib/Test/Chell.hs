{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Test.Chell
	(
	
	-- * Main
	  defaultMain
	
	-- * Test suites
	, Suite
	, suiteName
	, suiteTests
	
	-- ** Building test suites
	, BuildSuite
	, SuiteOrTest
	, suite
	
	-- ** Skipping some tests
	, skipIf
	, skipWhen
	
	-- * Basic testing library
	, Assertions
	, assertions
	, assert
	, expect
	, die
	, trace
	, note
	, afterTest
	, requireLeft
	, requireRight
	
	-- ** Built-in assertions
	, equal
	, notEqual
	, equalWithin
	, just
	, nothing
	, left
	, right
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
	
	-- ** Custom Assertions
	, IsAssertion
	, Assertion
	, assertionPassed
	, assertionFailed
	
	-- * Constructing tests
	, Test
	, test
	, testName
	, runTest
	
	, TestOptions
	, defaultTestOptions
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

import qualified Language.Haskell.TH as TH

import           Test.Chell.Main (defaultMain)
import           Test.Chell.Types

-- | Conditionally skip tests. Use this to avoid commenting out tests
-- which are currently broken, or do not work on the current platform.
--
-- @
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
--tests = 'suite' \"tests\"
--    test_Foo
--    ('skipWhen' noNetwork test_PingGoogle)
--    test_Bar
-- @
skipWhen :: SuiteOrTest a => IO Bool -> a -> a
skipWhen = skipWhen_

-- $doc-basic-testing
--
-- This library includes a few basic JUnit-style assertions, for use in
-- simple test suites where depending on a separate test framework is too
-- much trouble.

data Assertion
	= AssertionPassed
	| AssertionFailed String

assertionPassed :: Assertion
assertionPassed = AssertionPassed

assertionFailed :: String -> Assertion
assertionFailed = AssertionFailed

class IsAssertion a where
	runAssertion :: a -> IO Assertion

instance IsAssertion Assertion where
	runAssertion = return

instance IsAssertion Bool where
	runAssertion x = return $ if x
		then assertionPassed
		else assertionFailed "boolean assertion failed"

instance IsAssertion a => IsAssertion (IO a) where
	runAssertion x = x >>= runAssertion

type TestState = (IORef [(String, String)], IORef [IO ()], [Failure])
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
assertions :: String -> Assertions a -> Test
assertions name testm = test name $ \opts -> do
	noteRef <- newIORef []
	afterTestRef <- newIORef []
	
	let getNotes = fmap reverse (readIORef noteRef)
	
	let getResult = do
		res <- unAssertions testm (noteRef, afterTestRef, [])
		case res of
			(_, (_, _, [])) -> do
				notes <- getNotes
				return (TestPassed notes)
			(_, (_, _, fs)) -> do
				notes <- getNotes
				return (TestFailed notes (reverse fs))
	
	Control.Exception.finally
		(handleJankyIO opts getResult getNotes)
		(runAfterTest afterTestRef)

runAfterTest :: IORef [IO ()] -> IO ()
runAfterTest ref = readIORef ref >>= loop where
	loop [] = return ()
	loop (io:ios) = Control.Exception.finally (loop ios) io

addFailure :: Maybe TH.Loc -> String -> Assertions ()
addFailure maybe_loc msg = Assertions $ \(notes, afterTestRef, fs) -> do
	let loc = do
		th_loc <- maybe_loc
		return $ location
			{ locationFile = TH.loc_filename th_loc
			, locationModule = TH.loc_module th_loc
			, locationLine = toInteger (fst (TH.loc_start th_loc))
			}
	let f = failure
		{ failureLocation = loc
		, failureMessage = msg
		}
	return (Just (), (notes, afterTestRef, f : fs))

-- | Cause a test to immediately fail, with a message.
--
-- 'die' is a Template Haskell macro, to retain the source-file location from
-- which it was used. Its effective type is:
--
-- @
-- $die :: 'String' -> 'Assertions' a
-- @
die :: TH.Q TH.Exp
die = do
	loc <- TH.location
	let qloc = liftLoc loc
	[| \msg -> dieAt $qloc ("die: " ++ msg) |]

dieAt :: TH.Loc -> String -> Assertions a
dieAt loc msg = do
	addFailure (Just loc) msg
	Assertions (\s -> return (Nothing, s))

-- | Print a message from within a test. This is just a helper for debugging,
-- so you don't have to import @Debug.Trace@.
--
-- 'trace' is a Template Haskell macro, to retain the source-file location
-- from which it was used. Its effective type is:
--
-- @
-- $trace :: 'String' -> 'Assertions' ()
-- @
trace :: TH.Q TH.Exp
trace = do
	loc <- TH.location
	let qloc = liftLoc loc
	[| traceAt $qloc |]

traceAt :: TH.Loc -> String -> Assertions ()
traceAt loc msg = liftIO $ do
	let file = TH.loc_filename loc
	let line = fst (TH.loc_start loc)
	putStr ("[" ++ file ++ ":" ++ show line ++ "] ")
	putStrLn msg

-- | Attach metadata to a test run. This will be included in reports.
note :: String -> String -> Assertions ()
note key value = Assertions (\(notes, afterTestRef, fs) -> do
	modifyIORef notes ((key, value) :)
	return (Just (), (notes, afterTestRef, fs)))

-- | Register an IO action to be run after the test completes. This action
-- will run even if the test failed or threw an exception.
afterTest :: IO () -> Assertions ()
afterTest io = Assertions (\(notes, ref, fs) -> do
	modifyIORef ref (io :)
	return (Just (), (notes, ref, fs)))

-- | Require an 'Either' value to be 'Left', and return its contents. If
-- the value is 'Right', fail the test.
--
-- 'requireLeft' is a Template Haskell macro, to retain the source-file
-- location from which it was used. Its effective type is:
--
-- @
-- $requireLeft :: 'Show' b => 'Either' a b -> 'Assertions' a
-- @
requireLeft :: TH.Q TH.Exp
requireLeft = do
	loc <- TH.location
	let qloc = liftLoc loc
	[| requireLeftAt $qloc |]

requireLeftAt :: Show b => TH.Loc -> Either a b -> Assertions a
requireLeftAt loc val = case val of
	Left a -> return a
	Right b -> do
		let dummy = Right b `asTypeOf` Left ()
		dieAt loc ("requireLeft: received " ++ showsPrec 11 dummy "")

-- | Require an 'Either' value to be 'Right', and return its contents. If
-- the value is 'Left', fail the test.
--
-- 'requireRight' is a Template Haskell macro, to retain the source-file
-- location from which it was used. Its effective type is:
--
-- @
-- $requireRight :: 'Show' a => 'Either' a b -> 'Assertions' b
-- @
requireRight :: TH.Q TH.Exp
requireRight = do
	loc <- TH.location
	let qloc = liftLoc loc
	[| requireRightAt $qloc |]

requireRightAt :: Show a => TH.Loc -> Either a b -> Assertions b
requireRightAt loc val = case val of
	Left a -> do
		let dummy = Left a `asTypeOf` Right ()
		dieAt loc ("requireRight: received " ++ showsPrec 11 dummy "")
	Right b -> return b

liftLoc :: TH.Loc -> TH.Q TH.Exp
liftLoc loc = [| TH.Loc filename package module_ start end |] where
	filename = TH.loc_filename loc
	package = TH.loc_package loc
	module_ = TH.loc_module loc
	start = TH.loc_start loc
	end = TH.loc_end loc

assertAt :: IsAssertion assertion => TH.Loc -> Bool -> assertion -> Assertions ()
assertAt loc fatal assertion = do
	result <- liftIO (runAssertion assertion)
	case result of
		AssertionPassed -> return ()
		AssertionFailed err -> if fatal
			then dieAt loc err
			else addFailure (Just loc) err

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
pure True  _   = assertionPassed
pure False err = AssertionFailed err

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

-- | Assert that some value is @Left@.
left :: Either a b -> Assertion
left x = pure (isLeft x) ("left: received Right") where
	isLeft (Left _) = True
	isLeft (Right _) = False

-- | Assert that some value is @Right@.
right :: Either a b -> Assertion
right x = pure (isRight x) ("right: received Left") where
	isRight (Left _) = False
	isRight (Right _) = True

-- | Assert that some computation throws an exception matching the provided
-- predicate. This is mostly useful for exception types which do not have an
-- instance for @Eq@, such as @'Control.Exception.ErrorCall'@.
throws :: Exception err => (err -> Bool) -> IO a -> IO Assertion
throws p io = do
	either_exc <- Control.Exception.try io
	return $ case either_exc of
		Left exc -> if p exc
			then assertionPassed
			else assertionFailed ("throws: exception " ++ show exc ++ " did not match predicate")
		Right _ -> assertionFailed "throws: no exception thrown"

-- | Assert that some computation throws an exception equal to the given
-- exception. This is better than just checking that the correct type was
-- thrown, because the test can also verify the exception contains the correct
-- information.
throwsEq :: (Eq err, Exception err, Show err) => err -> IO a -> IO Assertion
throwsEq expected io = do
	either_exc <- Control.Exception.try io
	return $ case either_exc of
		Left exc -> if exc == expected
			then assertionPassed
			else assertionFailed ("throwsEq: exception " ++ show exc ++ " is not equal to " ++ show expected)
		Right _ -> assertionFailed "throwsEq: no exception thrown"

-- | Assert a value is greater than another.
greater :: (Ord a, Show a) => a -> a -> Assertion
greater x y = pure (x > y) ("greater: " ++ show x ++ " is not greater than " ++ show y)

-- | Assert a value is greater than or equal to another.
greaterEqual :: (Ord a, Show a) => a -> a -> Assertion
greaterEqual x y = pure (x >= y) ("greaterEqual: " ++ show x ++ " is not greater than or equal to " ++ show y)

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
