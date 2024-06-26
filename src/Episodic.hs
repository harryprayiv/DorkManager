{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
module Episodic where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Conduit (yieldMany, mapC)
import Data.Conduit.Binary (sinkFileCautious)

data Episodic = Episodic
  { title :: String
  , originalTitle :: String
  , sortTitle :: String
  , set :: String
  , year :: Int
  , rating :: Double
  , votes :: Int
  , mpaa :: String
  , movieId :: String
  , plot :: String
  , tagline :: String
  , runtime :: Int
  , genres :: [String]
  , tags :: [String]
  , countries :: [String]
  , studios :: [String]
  , directors :: [String]
  , credits :: [String]
  , fileInfo :: FileInfo
  , imdbId :: String
  , tmdbId :: String
  , actors :: [Actor]
  } deriving (Show, Generic)

instance ToJSON Episodic
instance FromJSON Episodic

data FileInfo = FileInfo
  { streamDetails :: StreamDetails
  } deriving (Show, Generic)

instance ToJSON FileInfo
instance FromJSON FileInfo

data StreamDetails = StreamDetails
  { video :: Video
  , audios :: [Audio]
  , subtitles :: [Subtitle]
  } deriving (Show, Generic)

instance ToJSON StreamDetails
instance FromJSON StreamDetails

data Video = Video
  { v_codec :: String
  , aspect :: Double
  , width :: Int
  , height :: Int
  } deriving (Show, Generic)

instance ToJSON Video
instance FromJSON Video

data Audio = Audio
  { a_codec :: String
  , a_language :: String
  , channels :: Int
  } deriving (Show, Generic)

instance ToJSON Audio
instance FromJSON Audio

data Subtitle = Subtitle
  { s_language :: String
  } deriving (Show, Generic)

instance ToJSON Subtitle
instance FromJSON Subtitle

data Actor = Actor
  { name :: String
  , role :: String
  } deriving (Show, Generic)

instance ToJSON Actor
instance FromJSON Actor
