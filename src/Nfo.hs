{-# LANGUAGE OverloadedStrings #-}

import Text.XML
import Text.XML.Cursor
import Data.Text (Text, unpack)
import qualified Data.Text as T

parseNfo :: Document -> Movie
parseNfo doc = Movie
  { title = getElemText "title"
  , originalTitle = getElemText "originaltitle"
  , sortTitle = getElemText "sorttitle"
  , year = read (getElemText "year") :: Int
  , rating = read (getElemText "rating") :: Double
  , votes = read (getElemText "votes") :: Int
  , mpaa = getElemText "mpaa"
  , movieId = getElemText "id"
  , plot = getElemText "plot"
  , tagline = getElemText "tagline"
  , runtime = read (getElemText "runtime") :: Int
  , genres = getElemsText "genre"
  , tags = getElemsText "tag"
  , countries = getElemsText "country"
  , studios = getElemsText "studio"
  , actors = getActors
  , directors = getElemsText "director"
  , credits = getElemsText "credits"
  , fileInfo = getFileInfo
  , imdbId = getAttrText "imdb" "id"
  , tmdbId = getAttrText "tmdb" "id"
  }
  where
    cursor = fromDocument doc
    getElemText name = T.unpack $ head $ cursor $// element name &// content
    getElemsText name = map T.unpack $ cursor $// element name &// content
    getAttrText elemName attrName = T.unpack $ head $ cursor $// element elemName >=> attribute attrName
    getActors = map parseActor $ cursor $// element "actor"
    getFileInfo = parseFileInfo $ head $ cursor $// element "fileinfo"

parseActor :: Cursor -> Actor
parseActor cur = Actor
  { name = unpack $ head $ cur $// element "name" &// content
  , role = unpack $ head $ cur $// element "role" &// content
  }

parseFileInfo :: Cursor -> FileInfo
parseFileInfo cur = FileInfo
  { streamDetails = parseStreamDetails $ head $ cur $// element "streamdetails"
  }

parseStreamDetails :: Cursor -> StreamDetails
parseStreamDetails cur = StreamDetails
  { video = parseVideo $ head $ cur $// element "video"
  , audios = map parseAudio $ cur $// element "audio"
  , subtitles = map parseSubtitle $ cur $// element "subtitle"
  }

parseVideo :: Cursor -> Video
parseVideo cur = Video
  { codec = getElemText "codec"
  , aspect = read (getElemText "aspect") :: Double
  , width = read (getElemText "width") :: Int
  , height = read (getElemText "height") :: Int
  }
  where
    getElemText name = T.unpack $ head $ cur $// element name &// content

parseAudio :: Cursor -> Audio
parseAudio cur = Audio
  { codec = getElemText "codec"
  , language = getElemText "language"
  , channels = read (getElemText "channels") :: Int
  }
  where
    getElemText name = T.unpack $ head $ cur $// element name &// content

parseSubtitle :: Cursor -> Subtitle
parseSubtitle cur = Subtitle
  { language = getElemText "language"
  }
  where
    getElemText name = T.unpack $ head $ cur $// element name &// content
