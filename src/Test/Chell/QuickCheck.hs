{-# LANGUAGE OverloadedStrings #-}

module Test.Chell.QuickCheck
	( property
	) where

import           Data.Text (Text, pack)

import           System.Random (mkStdGen)
import qualified Test.Chell as Chell
import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Gen as Gen
import qualified Test.QuickCheck.State as State
import qualified Test.QuickCheck.Test as Test
import qualified Test.QuickCheck.Text as Text

-- | Convert a QuickCheck property to a Chell 'Chell.Suite'.
--
-- @
--import Test.Chell
--import Test.Chell.QuickCheck
--import Test.QuickCheck hiding (property)
--
--tests :: [Suite]
--tests =
--    [ suite \"foo\"
--        [ property \"bar\" (\xs -> not (null xs) ==> length xs > 0)
--        ]
--    ]
-- @
property :: QuickCheck.Testable prop => Text -> prop -> Chell.Suite
property name prop = Chell.test (Chell.Test name chell_io) where
	chell_io opts = do
		let seed = Chell.testOptionSeed opts
		
		term <- Text.newNullTerminal
		
		let args = QuickCheck.stdArgs
		let state = State.MkState
			{ State.terminal = term
			, State.maxSuccessTests = QuickCheck.maxSuccess args
			, State.maxDiscardedTests = QuickCheck.maxDiscard args
			, State.computeSize = computeSize
			  	(QuickCheck.maxSize args)
			  	(QuickCheck.maxSuccess args)
			, State.numSuccessTests = 0
			, State.numDiscardedTests = 0
			, State.collected = []
			, State.expectedFailure = False
			, State.randomSeed = mkStdGen seed
			, State.numSuccessShrinks = 0
			, State.numTryShrinks = 0
			}
		
		result <- Test.test state (Gen.unGen (QuickCheck.property prop))
		let output = pack (Test.output result)
		return $ case result of
			Test.Success{} -> Chell.TestPassed
				[("seed", pack (show seed))]
			Test.Failure{} -> Chell.TestFailed
				[("seed", pack (show seed))]
				[Chell.Failure Nothing output]
			Test.GaveUp{} -> Chell.TestAborted
				[("seed", pack (show seed))]
				output
			Test.NoExpectedFailure{} -> Chell.TestFailed
				[("seed", pack (show seed))]
				[Chell.Failure Nothing output]

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
