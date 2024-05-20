{-# LANGUAGE DeriveGeneric #-}
module Movies where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

data Movie = Movie
  { title         :: String
  , originalTitle :: String
  , sortTitle     :: String
  , year          :: Int
  , rating        :: Double
  , votes         :: Int
  , mpaa          :: String
  , movieId       :: String
  , plot          :: String
  , tagline       :: String
  , runtime       :: Int
  , genres        :: [String]
  , tags          :: [String]
  , countries     :: [String]
  , studios       :: [String]
  , actors        :: [Actor]
  , directors     :: [String]
  , credits       :: [String]
  , fileInfo      :: FileInfo
  , imdbId        :: String
  , tmdbId        :: String
  } deriving (Show, Generic)

data Actor = Actor
  { name :: String
  , role :: String
  } deriving (Show, Generic)

data FileInfo = FileInfo
  { streamDetails :: StreamDetails
  } deriving (Show, Generic)

data StreamDetails = StreamDetails
  { video    :: Video
  , audios   :: [Audio]
  , subtitles :: [Subtitle]
  } deriving (Show, Generic)

data Video = Video
  { codec   :: String
  , aspect  :: Double
  , width   :: Int
  , height  :: Int
  } deriving (Show, Generic)

data Audio = Audio
  { codec    :: String
  , language :: String
  , channels :: Int
  } deriving (Show, Generic)

data Subtitle = Subtitle
  { language :: String
  } deriving (Show, Generic)

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
