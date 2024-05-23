module JSON where

import Data.Aeson (ToJSON, encode)
import System.FilePath
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Conduit (ConduitT, mapC, yieldMany, (.|), runConduitRes)
import Data.Conduit.Binary (sinkFileCautious)
import Movies

writeJsonIncremental :: (ToJSON a) => FilePath -> [a] -> IO ()
writeJsonIncremental filePath items = runConduitRes $
  yieldMany items .| mapC (BS.concat . BL.toChunks . encode) .| sinkFileCautious filePath

movieToJSON :: Movie -> BL.ByteString
movieToJSON = encode

writeJson :: (ToJSON a) => FilePath -> a -> IO ()
writeJson filePath = BL.writeFile filePath . encode
