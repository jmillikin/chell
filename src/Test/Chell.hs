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
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Maybe (isJust, isNothing)
import qualified Data.Text
import           Data.Text (Text)
import qualified Data.Text.IO

import qualified Language.Haskell.TH as TH

newtype Test = Test { runTest :: IO TestResult }

data TestResult
	= TestResultSuccess
	| TestResultSkipped
	| TestResultFailure [Failure]
	| TestResultError Text

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

suiteTests :: Suite -> [Test]
suiteTests (Suite _ suites) = concatMap suiteTests suites
suiteTests (SuiteTest _ t) = [t]

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
			Left exc -> TestResultError (errorExc exc)
			Right (_, []) -> TestResultSuccess
			Right (_, fs) -> TestResultFailure fs
	
	errorExc :: Control.Exception.SomeException -> Text
	errorExc exc = Data.Text.pack ("Test aborted with exception: " ++ show exc)

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
