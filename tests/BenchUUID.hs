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
                bench "null"           $ whnf U.null u1,
                bench "null nil"       $ whnf U.null U.nil
                ],
            bgroup "conversion" [
                bench "toString"       $ nf U.toString u1,
                bench "fromString"     $ nf U.fromString s1,
                bench "toByteString"   $ nf U.toByteString u1,
                bench "fromByteString" $ nf U.fromByteString b1
                ],
            bgroup "generation" [
                bench "V1" $ nfIO U.nextUUID,
                bench "V4" $ nfIO (randomIO :: IO U.UUID),
                bench "V5" $ nf   (U.generateNamed U.namespaceURL) n1
                ]
            ]

#if !(MIN_VERSION_criterion(0,4,0))
nf f arg = B ( rnf . f) arg
whnf f arg = B f arg
#endif

#if !(MIN_VERSION_criterion(0,4,1))
nfIO act = rnf `fmap` act
#endif