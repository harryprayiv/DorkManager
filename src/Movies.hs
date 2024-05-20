{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
module Movies where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

data Movie where
  Movie :: {title :: String,
              originalTitle :: String,
              sortTitle :: String,
              year :: Int,
              rating :: Double,
              votes :: Int,
              mpaa :: String,
              movieId :: String,
              plot :: String,
              tagline :: String,
              runtime :: Int,
              genres :: [String],
              tags :: [String],
              countries :: [String],
              studios :: [String],
              actors :: [Actor],
              directors :: [String],
              credits :: [String],
              fileInfo :: FileInfo,
              imdbId :: String,
              tmdbId :: String}
             -> Movie
  deriving (Show, Generic)

data Actor where
  Actor :: {name :: String, role :: String} -> Actor
  deriving (Show, Generic)

data FileInfo where
  FileInfo :: {streamDetails :: StreamDetails} -> FileInfo
  deriving (Show, Generic)

data StreamDetails where
  StreamDetails :: {video :: Video,
                      audios :: [Audio],
                      subtitles :: [Subtitle]}
                     -> StreamDetails
  deriving (Show, Generic)

data Video where
  Video :: {v_codec :: String,
              aspect :: Double,
              width :: Int,
              height :: Int}
             -> Video
  deriving (Show, Generic)

data Audio where
  Audio :: {a_codec :: String, a_language :: String, channels :: Int}
             -> Audio
  deriving (Show, Generic)

data Subtitle where
  Subtitle :: {s_language :: String} -> Subtitle
  deriving (Show, Generic)

instance ToJSON Movie
instance FromJSON Movie
instance ToJSON Actor
instance FromJSON Actor
instance ToJSON FileInfo
instance FromJSON FileInfo
instance ToJSON StreamDetails
instance FromJSON StreamDetails
instance ToJSON Video
instance FromJSON Video
instance ToJSON Audio
instance FromJSON Audio
instance ToJSON Subtitle
instance FromJSON Subtitle
