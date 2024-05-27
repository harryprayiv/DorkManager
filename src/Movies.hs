{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Movies where

import Data.Aeson (ToJSON, FromJSON, (.=), object, toJSON, toEncoding, pairs)
import GHC.Generics (Generic)

data Movie = Movie
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
  , videoFilePath :: String
  , checksum :: String
  } deriving (Show, Generic, Eq)

instance FromJSON Movie

instance ToJSON Movie where
  toJSON Movie{..} = 
    object [ "movieId" .= movieId
           , "title" .= title
           , "originalTitle" .= originalTitle
           , "sortTitle" .= sortTitle
           , "set" .= set
           , "year" .= year
           , "rating" .= rating
           , "votes" .= votes
           , "mpaa" .= mpaa
           , "plot" .= plot
           , "tagline" .= tagline
           , "runtime" .= runtime
           , "genres" .= genres
           , "tags" .= tags
           , "countries" .= countries
           , "studios" .= studios
           , "directors" .= directors
           , "credits" .= credits
           , "fileInfo" .= fileInfo
           , "imdbId" .= imdbId
           , "tmdbId" .= tmdbId
           , "videoFilePath" .= videoFilePath
           , "checksum" .= checksum
           , "actors" .= actors
           ]

  toEncoding Movie{..} =
    pairs $ "movieId" .= movieId
         <> "title" .= title
         <> "checksum" .= checksum
         <> "year" .= year
         <> "runtime" .= runtime
         <> "set" .= set
         <> "imdbId" .= imdbId
         <> "tmdbId" .= tmdbId
         <> "fileInfo" .= fileInfo
         <> "videoFilePath" .= videoFilePath
         <> "originalTitle" .= originalTitle
         <> "sortTitle" .= sortTitle
         <> "rating" .= rating
         <> "votes" .= votes
         <> "mpaa" .= mpaa
         <> "plot" .= plot
         <> "tagline" .= tagline
         <> "genres" .= genres
         <> "tags" .= tags
         <> "countries" .= countries
         <> "studios" .= studios
         <> "directors" .= directors   
         <> "credits" .= credits
         <> "actors" .= actors

data FileInfo = FileInfo
  { streamDetails :: StreamDetails
  } deriving (Show, Generic, Eq)

instance ToJSON FileInfo
instance FromJSON FileInfo

data StreamDetails = StreamDetails
  { video :: Video
  , audios :: [Audio]
  , subtitles :: [Subtitle]
  } deriving (Show, Generic, Eq)

instance ToJSON StreamDetails
instance FromJSON StreamDetails

data Video = Video
  { v_codec :: String
  , aspect :: Double
  , width :: Int
  , height :: Int
  } deriving (Show, Generic, Eq)

instance ToJSON Video
instance FromJSON Video

data Audio = Audio
  { a_codec :: String
  , a_language :: String
  , channels :: Int
  } deriving (Show, Generic, Eq)

instance ToJSON Audio
instance FromJSON Audio

data Subtitle = Subtitle
  { s_language :: String
  } deriving (Show, Generic, Eq)

instance ToJSON Subtitle
instance FromJSON Subtitle

data Actor = Actor
  { name :: String
  , role :: String
  } deriving (Show, Generic, Eq)

instance ToJSON Actor
instance FromJSON Actor
