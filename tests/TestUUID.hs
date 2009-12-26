import qualified Data.ByteString.Lazy as B
import Data.Maybe
import qualified Data.UUID as U
import qualified Data.UUID.V5 as U
import Test.HUnit
import Test.QuickCheck
import System.IO
import System.Random


instance Arbitrary U.UUID where
    arbitrary = (fst . random) `fmap` rand
    coarbitrary = undefined

test_null = TestList [
    "nil is null"              ~: assertBool "" (U.null U.nil),
    "namespaceDNS is not null" ~: assertBool "" (not $ U.null U.namespaceDNS)
    ]

test_nil = TestList [
    "nil string" ~: U.toString U.nil @?= "00000000-0000-0000-0000-000000000000",
    "nil bytes"  ~: U.toByteString U.nil @?= B.pack (replicate 16 0)
    ]

test_conv = TestList [
    "conv bytes to string" ~:
        maybe "" (U.toString) (U.fromByteString b16) @?= s16,
    "conv string to bytes" ~:
        maybe B.empty (U.toByteString) (U.fromString s16) @?= b16
    ]
    where b16 = B.pack [1..16]
          s16 = "01020304-0506-0708-090a-0b0c0d0e0f10"

prop_stringRoundTrip = label "String round trip" stringRoundTrip
    where stringRoundTrip :: U.UUID -> Bool
          stringRoundTrip u = maybe False (== u) $ U.fromString (U.toString u)

prop_byteStringRoundTrip = label "ByteString round trip" byteStringRoundTrip
    where byteStringRoundTrip :: U.UUID -> Bool
          byteStringRoundTrip u = maybe False (== u)
                                    $ U.fromByteString (U.toByteString u)

prop_stringLength = label "String length" stringLength
    where stringLength :: U.UUID -> Bool
          stringLength u = length (U.toString u) == 36

prop_byteStringLength = label "ByteString length" byteStringLength
    where byteStringLength :: U.UUID -> Bool
          byteStringLength u = B.length (U.toByteString u) == 16


main :: IO ()
main = do
    runTestText (putTextToHandle stderr False) (TestList [
        test_null,
        test_nil,
        test_conv
        ])
    mapM_ quickCheck $ [
        prop_stringRoundTrip,
        prop_byteStringRoundTrip,
        prop_stringLength,
        prop_byteStringLength
        ]
