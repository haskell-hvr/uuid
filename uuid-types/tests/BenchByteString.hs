import           Control.Applicative                ((<*>))
import           Data.Functor ((<$>))

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC8
import           System.Random
import           Test.QuickCheck

import           Criterion
import           Criterion.Main

import           Data.UUID.Types

instance Arbitrary UUID where
    arbitrary =
        fromWords <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- Testing what we do in the codebase
fromASCIIBytes_naive :: ByteString -> Maybe UUID
fromASCIIBytes_naive = fromString . BC8.unpack

toASCIIBytes_naive :: UUID -> ByteString
toASCIIBytes_naive = BC8.pack . toString

randomUUIDs :: IO [UUID]
randomUUIDs = sample' arbitrary

randomCorrect :: IO [String]
randomCorrect = map toString <$> randomUUIDs

randomSlightlyWrong :: IO [String]
randomSlightlyWrong = mapM screw =<< randomCorrect
  where
    screw s = do
        ix <- randomRIO (0, length s - 1)
        return (take ix s ++ "x" ++ drop (ix + 1) s)

randomVeryWrong :: IO [String]
randomVeryWrong = sample' arbitrary

main :: IO ()
main = do
    uuids <- randomUUIDs
    correct <- randomCorrect
    let correctBytes = map BC8.pack correct
    slightlyWrong <- randomSlightlyWrong
    let slightlyWrongBytes = map BC8.pack slightlyWrong
    veryWrong <- randomVeryWrong
    let veryWrongBytes = map BC8.pack veryWrong

    defaultMain
        [ bgroup "decoding"
          [ bcompare
            [ bgroup "correct"
              [ bench "fromASCIIBytes"       (nf (map fromASCIIBytes) correctBytes)
              , bench "fromString"           (nf (map fromString) correct)
              , bench "fromASCIIBytes_naive" (nf (map fromASCIIBytes_naive) correctBytes)
              ]
            ]
          , bcompare
            [ bgroup "slightly wrong"
              [ bench "fromASCIIBytes"       (nf (map fromASCIIBytes) slightlyWrongBytes)
              , bench "fromString"           (nf (map fromString) slightlyWrong)
              , bench "fromASCIIBytes_naive" (nf (map fromASCIIBytes_naive) slightlyWrongBytes)
              ]
            ]
          , bcompare
            [ bgroup "very wrong"
              [ bench "fromASCIIBytes"       (nf (map fromASCIIBytes) veryWrongBytes)
              , bench "fromString"           (nf (map fromString) veryWrong)
              , bench "fromASCIIBytes_naive" (nf (map fromASCIIBytes_naive) veryWrongBytes)
              ]
            ]
          ]
        , bcompare
          [ bgroup "encoding"
            [ bench "toASCIIBytes"       (nf (map toASCIIBytes) uuids)
            , bench "toString"           (nf (map toString) uuids)
            , bench "toASCIIBytes_naive" (nf (map toASCIIBytes_naive) uuids)
            ]
          ]
        ]
