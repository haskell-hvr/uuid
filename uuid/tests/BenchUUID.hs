import Gauge.Main
import Data.Char (ord)
import Data.IORef
import Data.Word
import qualified Data.UUID as U
import qualified Data.UUID.V1 as U
import qualified Data.UUID.V4 as U
import qualified Data.UUID.V3 as U3
import qualified Data.UUID.V5 as U5
import System.Random
import System.Random.Mersenne.Pure64

main :: IO ()
main = do
        let n1 = (map (fromIntegral . ord) "http://www.haskell.org/") :: [Word8]

        -- setup for random generation
        mtGenRef <- newPureMT >>= newIORef
        stdGenRef <- newStdGen >>= newIORef
        let randomUUID genRef = do
              state <- readIORef genRef
              let (uuid, state') = random state
              writeIORef genRef state'
              return uuid

        -- benchmark UUID generation
        defaultMain [
            bgroup "generation" [
                bench "V1" $ nfIO U.nextUUID,
                bench "V4-stock" $ nfIO (U.nextRandom :: IO U.UUID),
                bench "V4-mersenne" $ nfIO (randomUUID mtGenRef :: IO U.UUID),
                bench "V4-stdgen" $ nfIO (randomUUID stdGenRef :: IO U.UUID),
                bench "V3" $ nf   (U3.generateNamed U3.namespaceURL) n1,
                bench "V5" $ nf   (U5.generateNamed U5.namespaceURL) n1
                ]
            ]
