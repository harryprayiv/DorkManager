module JSON where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as B
import Movies

movieToJSON :: Movie -> B.ByteString
movieToJSON = encode