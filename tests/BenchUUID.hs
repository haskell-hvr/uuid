import Control.Monad (replicateM)
import Control.Parallel.Strategies
import Criterion.Main
import Data.Char (ord)
import Data.Maybe (fromJust)
import Data.Word
import qualified Data.Set as Set
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
                ],
            bench "set making" $ nf Set.fromList uuids
            ]

-- 50 uuids, so tests can be repeatable
uuids :: [U.UUID]
uuids 
    = map (fromJust . U.fromString)
        [
         "35d42593-1fca-4465-b588-a2e78cb996ba",
         "1e97e407-eca7-4c5d-a947-6fbe9dc168b6",
         "a41fd7ce-a053-4c0a-a742-77c95b85da2a",
         "f7e3913a-0fd7-4355-a92f-d73f9b046efa",
         "8961a35d-55c2-42f3-8680-08fce3986647",
         "96246c58-d0b4-4e56-a543-356bd59686a9",
         "72c46194-648c-4b1e-a9fb-2eba060ab43b",
         "0fc252d4-a37b-4eca-9309-e3d2a59a3a22",
         "a8aceb5a-6a8e-43f3-85bb-9653a3c1ebcd",
         "b23d1118-6bc8-4add-9d56-99634d78949f",
         "5f8c7896-9c4f-4d7e-a4d2-961bda298012",
         "219a4137-7bc5-42b1-ac95-490948a978e1",
         "5af5024f-fbe9-45ab-991d-49b655994437",
         "569dfb33-185d-4a3c-99c4-bc2b83250a7c",
         "43a58442-aa51-4a5d-8e00-b8a83b5fc5b0",
         "2865ced1-b54d-4725-8f01-b408f4617424",
         "b8cfaff0-4dee-4f32-af2f-0469b3e535fc",
         "63f45bc0-f303-4f1a-b0d6-76f876be626d",
         "d171eaf3-f20d-45d6-9268-0cc22dfbe887",
         "7c28f457-ad27-494a-8642-6e47e7f3efb8",
         "f8de9193-66ff-49e8-9826-fc50858d7855",
         "2af85f28-cace-4740-b8bb-2b5860f5fdb3",
         "b12a7a22-edb3-4694-b8f4-0532eaad6112",
         "5e052a08-8e49-4668-bbe7-77cdbc4679a3",
         "42d5a68d-3f08-4e39-9b8c-71cc17c538ed",
         "ba2b1487-b3a4-4a19-98cc-59530f36613f",
         "e4a5c569-b8ac-4851-8ad2-fbdb89986be2",
         "35b4a1b6-b5ca-4646-803b-c337ee730d9a",
         "f0df1206-05d9-49d6-a726-d49fb253e645",
         "9656a0f0-89b2-4cec-bb1c-fdf5633e1cd7",
         "d13382f5-04a1-422d-94d6-e47219425816",
         "b8d0c762-4c6b-4bd0-ad0b-f68988a87166",
         "06360e85-f18a-44f6-aa72-45f1e60b6b77",
         "347491ca-62b3-48e8-ac94-6ffebe1318ed",
         "014339d0-7b2d-4dda-914b-14ee5cb4391b",
         "dc57931b-4744-41be-b3c4-e24dbeeb606a",
         "2c9fdcf0-e1d6-4a2d-951d-4766b99032cb",
         "b6bde422-eea8-4231-bf1c-ee0e56699511",
         "921073c5-f7c1-4583-ac03-aeb7aef8662b",
         "eff6a517-aeb1-453e-9810-1b4b324c5a1e",
         "eadaae8c-cbf8-4e0b-ab80-e34284b07dad",
         "4d307c36-70d9-453f-8455-ec7e2ba405ed",
         "53ddef9a-2413-4f7f-8363-cff56ee17c6c",
         "4dd4c27a-b300-4a00-ab87-eef505275492",
         "4a5d7001-f0c1-4e2f-8362-8833bda2114b",
         "0c46f438-9365-406a-b04e-34436806ec25",
         "b35469cf-05f8-40ff-a38b-117604347957",
         "f1c54df0-5f59-4891-b3c6-82bac0d814ca",
         "8091837c-6456-42c3-a686-f4731a41d4f9",
         "2a0e2efb-a11c-4a44-81ee-3efc37379b48"
        ]

#if !(MIN_VERSION_criterion(0,4,0))
nf f arg = B ( rnf . f) arg
whnf f arg = B f arg
#endif

#if !(MIN_VERSION_criterion(0,4,1))
nfIO act = rnf `fmap` act
#endif