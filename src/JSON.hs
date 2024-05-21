module JSON where

import Data.Aeson (ToJSON, encode)
import System.FilePath
import qualified Data.ByteString.Lazy as BL
import Movies

writeJson :: (ToJSON a) => FilePath -> a -> IO ()
writeJson filePath = BL.writeFile filePath . encode

movieToJSON :: Movie -> BL.ByteString
movieToJSON = encode
