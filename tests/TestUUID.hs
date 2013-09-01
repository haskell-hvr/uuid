import Control.Monad (replicateM)
import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.Char (ord)
import Data.List (nub, (\\))
import Data.Maybe
import Data.Word
import qualified Data.UUID as U
import qualified Data.UUID.V1 as U
import qualified Data.UUID.V3 as U3
import qualified Data.UUID.V5 as U5
import qualified Test.HUnit as H
import Test.HUnit hiding (Test)
import Test.QuickCheck hiding ((.&.))
import Test.Framework (defaultMain, Test(..))
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import System.IO


isValidVersion :: Int -> U.UUID -> Bool
isValidVersion v u = lenOK && variantOK && versionOK
    where bs = U.toByteString u
          lenOK = B.length bs == 16
          variantOK = (B.index bs 8) .&. 0xc0 == 0x80
          versionOK = (B.index bs 6) .&. 0xf0 == fromIntegral (v `shiftL` 4)


instance Arbitrary U.UUID where
    -- the UUID random instance ignores bounds
    arbitrary = choose (U.nil, U.nil)


test_null :: H.Test
test_null = H.TestList [
    "nil is null"              ~: assertBool "" (U.null U.nil),
    "namespaceDNS is not null" ~: assertBool "" (not $ U.null U3.namespaceDNS)
    ]

test_nil :: H.Test
test_nil = H.TestList [
    "nil string" ~: U.toString U.nil @?= "00000000-0000-0000-0000-000000000000",
    "nil bytes"  ~: U.toByteString U.nil @?= B.pack (replicate 16 0)
    ]

test_conv :: H.Test
test_conv = H.TestList [
    "conv bytes to string" ~:
        maybe "" (U.toString) (U.fromByteString b16) @?= s16,
    "conv string to bytes" ~:
        maybe B.empty (U.toByteString) (U.fromString s16) @?= b16
    ]
    where b16 = B.pack [1..16]
          s16 = "01020304-0506-0708-090a-0b0c0d0e0f10"

test_v1 :: [Maybe U.UUID] -> H.Test
test_v1 v1s = H.TestList [
    "V1 unique" ~: nub (v1s \\ nub v1s) @?= [],
    "V1 not null" ~: H.TestList $ map (testUUID (not . U.null))  v1s,
    "V1 valid"    ~: H.TestList $ map (testUUID (isValidVersion 1)) v1s
    ]
    where testUUID :: (U.UUID -> Bool) -> Maybe U.UUID -> H.Test
          testUUID p u = maybe False p u ~? show u

test_v3 :: H.Test
test_v3 = H.TestList [
    "V3 computation" ~:
          U3.generateNamed U3.namespaceDNS name @?= uV3
    ]
    where name = map (fromIntegral . ord) "www.widgets.com" :: [Word8]
          uV3 = fromJust $ U.fromString "3d813cbb-47fb-32ba-91df-831e1593ac29"

test_v5 :: H.Test
test_v5 = H.TestList [
    "V5 computation" ~:
          U5.generateNamed U5.namespaceDNS name @?= uV5
    ]
    where name = map (fromIntegral . ord) "www.widgets.com" :: [Word8]
          uV5 = fromJust $ U.fromString "21f7f8de-8051-5b89-8680-0195ef798b6a"

prop_stringRoundTrip :: Test
prop_stringRoundTrip = testProperty "String round trip" stringRoundTrip
    where stringRoundTrip :: U.UUID -> Bool
          stringRoundTrip u = maybe False (== u) $ U.fromString (U.toString u)

prop_byteStringRoundTrip :: Test
prop_byteStringRoundTrip = testProperty "ByteString round trip" byteStringRoundTrip
    where byteStringRoundTrip :: U.UUID -> Bool
          byteStringRoundTrip u = maybe False (== u)
                                    $ U.fromByteString (U.toByteString u)

prop_stringLength :: Test
prop_stringLength = testProperty "String length" stringLength
    where stringLength :: U.UUID -> Bool
          stringLength u = length (U.toString u) == 36

prop_byteStringLength :: Test
prop_byteStringLength = testProperty "ByteString length" byteStringLength
    where byteStringLength :: U.UUID -> Bool
          byteStringLength u = B.length (U.toByteString u) == 16

prop_randomsDiffer :: Test
prop_randomsDiffer = testProperty "Randoms differ" randomsDiffer
    where randomsDiffer :: (U.UUID, U.UUID) -> Bool
          randomsDiffer (u1, u2) = u1 /= u2

prop_randomNotNull :: Test
prop_randomNotNull = testProperty "Random not null" randomNotNull
    where randomNotNull :: U.UUID -> Bool
          randomNotNull = not. U.null

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

prop_readShowRoundTrip :: Test
prop_readShowRoundTrip = testProperty "Read/Show round-trip" prop
    where -- we're using 'Maybe UUID' to add a bit of
          -- real-world complexity.
          prop :: U.UUID -> Bool
          prop uuid = read (show (Just uuid)) == Just uuid

main :: IO ()
main = do
    v1s <- replicateM 100 U.nextUUID
    defaultMain $
     concat $
     [ hUnitTestToTests $ H.TestList [
        test_null,
        test_nil,
        test_conv,
        test_v1 v1s,
        test_v3,
        test_v5
        ]
     , [ prop_stringRoundTrip,
         prop_readShowRoundTrip,
         prop_byteStringRoundTrip,
         prop_stringLength,
         prop_byteStringLength,
         prop_randomsDiffer,
         prop_randomNotNull,
         prop_randomsValid,
         prop_v3NotNull,
         prop_v3Valid,
         prop_v5NotNull,
         prop_v5Valid
         ]
        ]
