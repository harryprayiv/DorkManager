{-# LANGUAGE OverloadedStrings #-}

module NFO where

import Text.XML
import Text.XML.Cursor
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Maybe (listToMaybe)
import Movies

parseNfo :: Document -> Maybe Movie
parseNfo doc = do
  let cursor = fromDocument doc
  fileInfo <- parseFileInfo =<< listToMaybe (cursor $// element "fileinfo")
  return Movie
    { title = getElemText cursor "title"
    , originalTitle = getElemText cursor "originaltitle"
    , sortTitle = getElemText cursor "sorttitle"
    , set = getFirstLineOfElemText cursor "set"
    , year = readElemText cursor "year" 0
    , rating = readElemText cursor "rating" 0.0
    , votes = readElemText cursor "votes" 0
    , mpaa = getElemText cursor "mpaa"
    , movieId = getElemText cursor "id"
    , plot = getElemText cursor "plot"
    , tagline = getElemText cursor "tagline"
    , runtime = readElemText cursor "runtime" 0
    , genres = getElemsText cursor "genre"
    , tags = getElemsText cursor "tag"
    , countries = getElemsText cursor "country"
    , studios = getElemsText cursor "studio"
    , directors = getElemsText cursor "director"
    , credits = getElemsText cursor "credits"
    , fileInfo = fileInfo
    , imdbId = getAttrText cursor "imdb" "id"
    , tmdbId = getAttrText cursor "tmdb" "id"
    , actors = []  -- Initialize actors as an empty list
    }
  where
    getElemText cur name = T.unpack $ T.concat $ cur $// element name &// content
    getFirstLineOfElemText cur name = T.unpack . T.strip . head . T.lines $ T.concat $ cur $// element name &// content
    getElemsText cur name = map T.unpack $ cur $// element name &// content
    getAttrText cur elemName attrName = T.unpack $ T.concat $ cur $// element elemName >=> attribute attrName
    readElemText cur name def = maybe def id . readMaybe . getElemText cur $ name

parseFileInfo :: Cursor -> Maybe FileInfo
parseFileInfo cur = do
  streamDetails <- parseStreamDetails =<< listToMaybe (cur $// element "streamdetails")
  return FileInfo
    { streamDetails = streamDetails
    }

parseStreamDetails :: Cursor -> Maybe StreamDetails
parseStreamDetails cur = do
  video <- parseVideo =<< listToMaybe (cur $// element "video")
  let audios = map parseAudio (cur $// element "audio")
  let subtitles = map parseSubtitle (cur $// element "subtitle")
  return StreamDetails
    { video = video
    , audios = audios
    , subtitles = subtitles
    }

parseVideo :: Cursor -> Maybe Video
parseVideo cur = Just Video
  { v_codec = getElemText cur "codec"
  , aspect = readElemText cur "aspect" 0.0
  , width = readElemText cur "width" 0
  , height = readElemText cur "height" 0
  }
  where
    getElemText cur name = T.unpack $ T.concat $ cur $// element name &// content
    readElemText cur name def = maybe def id . readMaybe . getElemText cur $ name

parseAudio :: Cursor -> Audio
parseAudio cur = Audio
  { a_codec = getElemText cur "codec"
  , a_language = getElemText cur "language"
  , channels = readElemText cur "channels" 0
  }
  where
    getElemText cur name = T.unpack $ T.concat $ cur $// element name &// content
    readElemText cur name def = maybe def id . readMaybe . getElemText cur $ name

parseSubtitle :: Cursor -> Subtitle
parseSubtitle cur = Subtitle
  { s_language = getElemText cur "language"
  }
  where
    getElemText cur name = T.unpack $ T.concat $ cur $// element name &// content
