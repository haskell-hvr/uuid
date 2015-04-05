{-# LANGUAGE ViewPatterns #-}

import Control.Monad (replicateM)
import Data.Bits
import qualified Data.ByteString.Lazy as BL
import Data.Char (ord)
import Data.List (nub, (\\))
import Data.Maybe
import Data.Word
import qualified Data.UUID as U
import qualified Data.UUID.V1 as U
import qualified Data.UUID.V3 as U3
import qualified Data.UUID.V5 as U5

import Test.QuickCheck ( Arbitrary(arbitrary), choose )
import Test.Tasty ( TestTree, testGroup, defaultMain )
import Test.Tasty.HUnit
      ( Assertable(assert), assertBool, (@?=), testCase )
import Test.Tasty.QuickCheck ( testProperty )

type Test = TestTree

isValidVersion :: Int -> U.UUID -> Bool
isValidVersion v u = lenOK && variantOK && versionOK
    where bs = U.toByteString u
          lenOK = BL.length bs == 16
          variantOK = (BL.index bs 8) .&. 0xc0 == 0x80
          versionOK = (BL.index bs 6) .&. 0xf0 == fromIntegral (v `shiftL` 4)


instance Arbitrary U.UUID where
    -- the UUID random instance ignores bounds
    arbitrary = choose (U.nil, U.nil)


test_null :: Test
test_null =
  testCase "namespaceDNS is not null" $
  assertBool "" (not $ U.null U3.namespaceDNS)

test_v1 :: [Maybe U.UUID] -> Test
test_v1 v1s = testGroup "version 1" [
    testCase  "V1 unique"   $ nub (v1s \\ nub v1s) @?= [],
    testGroup "V1 not null" $ map (testUUID (not . U.null))  v1s,
    testGroup "V1 valid"    $ map (testUUID (isValidVersion 1)) v1s
    ]
    where testUUID :: (U.UUID -> Bool) -> Maybe U.UUID -> Test
          testUUID p u =
            testCase (show u) $
            assert $ maybe False p u

test_v3 :: Test
test_v3 =
    testCase "V3 computation" $
          U3.generateNamed U3.namespaceDNS name @?= uV3

  where name = map (fromIntegral . ord) "www.widgets.com" :: [Word8]
        uV3 = fromJust $ U.fromString "3d813cbb-47fb-32ba-91df-831e1593ac29"

test_v5 :: Test
test_v5 =
    testCase "V5 computation" $
          U5.generateNamed U5.namespaceDNS name @?= uV5

    where name = map (fromIntegral . ord) "www.widgets.com" :: [Word8]
          uV5 = fromJust $ U.fromString "21f7f8de-8051-5b89-8680-0195ef798b6a"


prop_randomsValid :: Test
prop_randomsValid = testProperty "Random valid" randomsValid
    where randomsValid :: U.UUID -> Bool
          randomsValid = isValidVersion 4

prop_v3NotNull :: Test
prop_v3NotNull = testProperty "V3 not null" v3NotNull
    where v3NotNull :: [Word8] -> Bool
          v3NotNull = not . U.null . U3.generateNamed U3.namespaceDNS

prop_v3Valid :: Test
prop_v3Valid = testProperty "V3 valid" v3Valid
    where v3Valid :: [Word8] -> Bool
          v3Valid = isValidVersion 3 . U3.generateNamed U3.namespaceDNS

prop_v5NotNull :: Test
prop_v5NotNull = testProperty "V5 not null" v5NotNull
    where v5NotNull :: [Word8] -> Bool
          v5NotNull = not . U.null . U5.generateNamed U5.namespaceDNS

prop_v5Valid :: Test
prop_v5Valid = testProperty "V5 valid" v5Valid
    where v5Valid :: [Word8] -> Bool
          v5Valid = isValidVersion 5 . U5.generateNamed U5.namespaceDNS

main :: IO ()
main = do
    v1s <- replicateM 100 U.nextUUID
    defaultMain $
     testGroup "tests" $
     concat $
     [ [
        test_null,
        test_v1 v1s,
        test_v3,
        test_v5
        ]
     , [ prop_randomsValid,
         prop_v3NotNull,
         prop_v3Valid,
         prop_v5NotNull,
         prop_v5Valid
         ]
     ]
