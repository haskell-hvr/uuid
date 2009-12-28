import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.Char (ord)
import Data.Maybe
import Data.Word
import qualified Data.UUID as U
import qualified Data.UUID.V5 as U
import Test.HUnit
import Test.QuickCheck
import System.IO
import System.Random


isValidVersion :: Int -> U.UUID -> Bool
isValidVersion v u = lenOK && variantOK && versionOK
    where bs = U.toByteString u
          lenOK = B.length bs == 16
          variantOK = (B.index bs 8) .&. 0xc0 == 0x80
          versionOK = (B.index bs 6) .&. 0xf0 == fromIntegral (v `shiftL` 4)


instance Arbitrary U.UUID where
    arbitrary = (fst . random) `fmap` rand
    coarbitrary = undefined

instance Arbitrary Word8 where
    arbitrary = (fromIntegral . fst . randomR (0,255::Int)) `fmap` rand
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

test_v5 = TestList [
    "V5 computation" ~:
          U.generateNamed U.namespaceDNS name @?= uV5
    ]
    where name = map (fromIntegral . ord) "www.widgets.com" :: [Word8]
    --    uV3 = fromJust $ U.fromString "3d813cbb-47fb-32ba-91df-831e1593ac29"
          uV5 = fromJust $ U.fromString "21f7f8de-8051-5b89-8680-0195ef798b6a"

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

prop_randomsDiffer :: Property
prop_randomsDiffer = label "Randoms differ" randomsDiffer
    where randomsDiffer :: (U.UUID, U.UUID) -> Bool
          randomsDiffer (u1, u2) = u1 /= u2

prop_randomNotNull :: Property
prop_randomNotNull = label "Random not null" randomNotNull
    where randomNotNull :: U.UUID -> Bool
          randomNotNull = not. U.null
          
prop_randomsValid :: Property
prop_randomsValid = label "Random valid" randomsValid
    where randomsValid :: U.UUID -> Bool
          randomsValid = isValidVersion 4

prop_v5NotNull :: Property
prop_v5NotNull = label "V5 not null" v5NotNull
    where v5NotNull :: [Word8] -> Bool
          v5NotNull = not . U.null . U.generateNamed U.namespaceDNS 

prop_v5Valid :: Property
prop_v5Valid = label "V5 valid" v5Valid
    where v5Valid :: [Word8] -> Bool
          v5Valid = isValidVersion 5 . U.generateNamed U.namespaceDNS 


main :: IO ()
main = do
    runTestText (putTextToHandle stderr False) (TestList [
        test_null,
        test_nil,
        test_conv,
        test_v5
        ])
    mapM_ quickCheck $ [
        prop_stringRoundTrip,
        prop_byteStringRoundTrip,
        prop_stringLength,
        prop_byteStringLength,
        prop_randomsDiffer,
        prop_randomNotNull,
        prop_randomsValid,
        prop_v5NotNull,
        prop_v5Valid
        ]
