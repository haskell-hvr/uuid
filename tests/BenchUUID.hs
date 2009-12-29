import Control.Monad (replicateM)
import Control.Parallel.Strategies
import Criterion.Main
import Data.Char (ord)
import Data.Maybe (fromJust)
import Data.Word
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BL
import qualified Data.UUID as U
import qualified Data.UUID.Internal as U
import qualified Data.UUID.V1 as U
import qualified Data.UUID.V5 as U
import System.Random


instance NFData BL.ByteString where
    rnf BL.Empty        = ()
    rnf (BL.Chunk _ ts) = rnf ts

instance NFData U.UUID where
    rnf (U.UUID tl tm th ch cl node) =
        rnf tl `seq` rnf tm `seq` rnf th `seq` rnf ch `seq` rnf cl `seq` rnf node

instance NFData U.Node where
    rnf (U.Node w1 w2 w3 w4 w5 w6) =
        rnf w1 `seq` rnf w2 `seq` rnf w3 `seq` rnf w4 `seq` rnf w5 `seq` rnf w6



main :: IO ()
main = do
        u1 <- randomIO
        let s1 = U.toString u1
            b1 = U.toByteString u1
            n1 = (map (fromIntegral . ord) "http://www.haskell.org/") :: [Word8]
        defaultMain [
            bgroup "null" [
                bench "null"           $ B U.null u1,
                bench "null nil"       $ B U.null U.nil
                ],
            bgroup "conversion" [
                bench "toString"       $ B (rnf . U.toString) u1,
                bench "fromString"     $ B (rnf . U.fromString) s1,
                bench "toByteString"   $ B (rnf . U.toByteString) u1,
                bench "fromByteString" $ B (rnf . U.fromByteString) b1
                ],
            bgroup "generation" [
                bench "V1" $ rnf `fmap` U.nextUUID,
                bench "V4" $ rnf `fmap` (randomIO :: IO U.UUID),
                bench "V5" $ B (rnf  . U.generateNamed U.namespaceURL) n1
                ]
            ]
