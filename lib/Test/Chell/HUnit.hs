module Test.Chell.HUnit
	( hunit
	) where

import           Data.Text (Text, pack)

import qualified Test.Chell as Chell
import           Test.HUnit.Lang (Assertion, performTestCase)

-- | Convert a sequence of HUnit assertions (embedded in IO) to a Chell
-- 'Chell.Suite'.
--
-- @
-- import Test.Chell
-- import Test.Chell.HUnit
-- import Test.HUnit
--
-- tests :: [Suite]
-- tests =
--     [ suite \"foo\"
--         [ hunit \"bar\" $ do
--             1 + 2 \@?= 3
--         ]
--     ]
-- @
hunit :: Text -> Assertion -> Chell.Suite
hunit name io = Chell.test (Chell.Test name chell_io) where
	chell_io _ = do
		result <- performTestCase io
		return $ case result of
			Nothing -> Chell.TestPassed []
			Just err -> parseError err
	parseError (True, msg) = Chell.TestFailed [] [Chell.Failure Nothing (pack msg)]
	parseError (False, msg) = Chell.TestAborted [] (pack msg)
