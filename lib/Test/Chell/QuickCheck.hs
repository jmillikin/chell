{-# LANGUAGE CPP #-}

module Test.Chell.QuickCheck
	( property
	) where

import           System.Random (mkStdGen)

import qualified Test.Chell as Chell
import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Gen as Gen
#if MIN_VERSION_QuickCheck(2,7,0)
import           Test.QuickCheck.Property (unProperty)
import qualified Test.QuickCheck.Random as QCRandom
#endif
import qualified Test.QuickCheck.State as State
import qualified Test.QuickCheck.Test as Test
import qualified Test.QuickCheck.Text as Text

-- | Convert a QuickCheck property to a Chell 'Chell.Test'.
--
-- @
--import Test.Chell
--import Test.Chell.QuickCheck
--import Test.QuickCheck hiding (property)
--
--test_NullLength :: Test
--test_NullLength = property \"null-length\"
--    (\xs -> not (null xs) ==> length xs > 0)
-- @
property :: QuickCheck.Testable prop => String -> prop -> Chell.Test
#if MIN_VERSION_QuickCheck(2,6,0)
property name prop = Chell.test name $ \opts ->
	Text.withNullTerminal $ \term -> do
#else
property name prop = Chell.test name $ \opts -> do
	term <- Text.newNullTerminal
#endif
	
	let seed = Chell.testOptionSeed opts
	
	let args = QuickCheck.stdArgs
	let state = State.MkState
		{ State.terminal = term
		, State.maxSuccessTests = QuickCheck.maxSuccess args
		, State.maxDiscardedTests = maxDiscardedTests args prop

		, State.computeSize = computeSize (QuickCheck.maxSize args) (QuickCheck.maxSuccess args)
		, State.numSuccessTests = 0
		, State.numDiscardedTests = 0
		, State.collected = []
		, State.expectedFailure = False

#if MIN_VERSION_QuickCheck(2,7,0)
		, State.randomSeed = QCRandom.mkQCGen seed
#else
		, State.randomSeed = mkStdGen seed
#endif
		, State.numSuccessShrinks = 0
		, State.numTryShrinks = 0
#if MIN_VERSION_QuickCheck(2,5,0)
		, State.numTotTryShrinks = 0
#endif
#if MIN_VERSION_QuickCheck(2,5,1)
		, State.numRecentlyDiscardedTests = 0
#endif
		}
	
#if MIN_VERSION_QuickCheck(2,7,0)
	let genProp = unProperty (QuickCheck.property prop)
#else
	let genProp = QuickCheck.property prop
#endif
	result <- Test.test state (Gen.unGen genProp)
	let output = Test.output result
	let notes = [("seed", show seed)]
	let failure = Chell.failure { Chell.failureMessage = output }
	return $ case result of
		Test.Success{} -> Chell.TestPassed notes
		Test.Failure{} -> Chell.TestFailed notes [failure]
		Test.GaveUp{} -> Chell.TestAborted notes output
		Test.NoExpectedFailure{} -> Chell.TestFailed notes [failure]

-- copied from quickcheck-2.4.1.1/src/Test/QuickCheck/Test.hs
computeSize :: Int -> Int -> Int -> Int -> Int
computeSize maxSize maxSuccess n d
	-- e.g. with maxSuccess = 250, maxSize = 100, goes like this:
	-- 0, 1, 2, ..., 99, 0, 1, 2, ..., 99, 0, 2, 4, ..., 98.
	| n `roundTo` maxSize + maxSize <= maxSuccess ||
	  n >= maxSuccess ||
	  maxSuccess `mod` maxSize == 0 = n `mod` maxSize + d `div` 10
	| otherwise =
	 (n `mod` maxSize) * maxSize `div` (maxSuccess `mod` maxSize) + d `div` 10

roundTo :: Int -> Int -> Int
roundTo n m = (n `div` m) * m

maxDiscardedTests :: QuickCheck.Testable prop => QuickCheck.Args -> prop -> Int
#if MIN_VERSION_QuickCheck(2,5,0)
maxDiscardedTests args p = if QuickCheck.exhaustive p
	then QuickCheck.maxDiscardRatio args
	else QuickCheck.maxDiscardRatio args * QuickCheck.maxSuccess args
#else
maxDiscardedTests args _ = QuickCheck.maxDiscard args
#endif
