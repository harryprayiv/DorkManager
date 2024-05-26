{-# LANGUAGE OverloadedStrings #-}

module NFO where

import Text.XML
import Text.XML.Cursor
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Maybe (listToMaybe, fromMaybe, mapMaybe)
import Debug.Trace (trace, traceShowId, traceShow)
import Movies

parseNfo :: Document -> Maybe Movie
parseNfo doc = do
  let cursor = fromDocument doc
  trace "Parsing fileInfo..." $ return ()
  fileInfo <- parseFileInfo =<< listToMaybe (cursor $// element (Name "fileinfo" Nothing Nothing))
  trace "Parsing Movie details..." $ return ()
  return Movie
    { title = traceShowId $ getElemText cursor (Name "title" Nothing Nothing)
    , originalTitle = traceShowId $ getElemText cursor (Name "originaltitle" Nothing Nothing)
    , sortTitle = traceShowId $ getElemText cursor (Name "sorttitle" Nothing Nothing)
    , set = traceShow ("Set: " ++ show (getFirstLineOfElemText cursor (Name "set" Nothing Nothing))) $ getFirstLineOfElemText cursor (Name "set" Nothing Nothing)
    , year = traceShowId $ readElemText cursor (Name "year" Nothing Nothing) 0
    , rating = traceShowId $ readElemText cursor (Name "rating" Nothing Nothing) 0.0
    , votes = traceShowId $ readElemText cursor (Name "votes" Nothing Nothing) 0
    , mpaa = traceShowId $ getElemText cursor (Name "mpaa" Nothing Nothing)
    , movieId = traceShowId $ getElemText cursor (Name "id" Nothing Nothing)
    , plot = traceShowId $ getElemText cursor (Name "plot" Nothing Nothing)
    , tagline = traceShowId $ getElemText cursor (Name "tagline" Nothing Nothing)
    , runtime = traceShowId $ readElemText cursor (Name "runtime" Nothing Nothing) 0
    , genres = traceShowId $ getElemsText cursor (Name "genre" Nothing Nothing)
    , tags = traceShowId $ getElemsText cursor (Name "tag" Nothing Nothing)
    , countries = traceShowId $ getElemsText cursor (Name "country" Nothing Nothing)
    , studios = traceShowId $ getElemsText cursor (Name "studio" Nothing Nothing)
    , directors = traceShowId $ getElemsText cursor (Name "director" Nothing Nothing)
    , credits = traceShowId $ getElemsText cursor (Name "credits" Nothing Nothing)
    , fileInfo = traceShowId $ fileInfo
    , imdbId = traceShowId $ getAttrText cursor (Name "imdb" Nothing Nothing) (Name "id" Nothing Nothing)
    , tmdbId = traceShowId $ getAttrText cursor (Name "tmdb" Nothing Nothing) (Name "id" Nothing Nothing)
    , actors = traceShowId $ parseActors cursor
    }
  where
    getElemText cur name = trace ("Getting element text for: " ++ show name) $
      let texts = cur $// element name &// content
      in T.unpack $ T.concat texts
    getFirstLineOfElemText cur name =
      let fullText = T.concat $ cur $// element name &// content
          lines = T.lines fullText
          firstLine = if null lines then "" else T.strip (head lines)
      in trace ("Extracting first line of element: " ++ show name ++ " -> " ++ T.unpack firstLine) $ T.unpack firstLine
    getElemsText cur name = trace ("Getting elements text for: " ++ show name) $ map T.unpack $ cur $// element name &// content
    getAttrText cur elemName attrName = trace ("Getting attribute text for: " ++ show elemName ++ " attribute: " ++ show attrName) $
      let attrs = cur $// element elemName >=> attribute attrName
      in T.unpack $ T.concat attrs
    readElemText cur name def = trace ("Reading element text for: " ++ show name) $
      let text = getElemText cur name
      in fromMaybe def (readMaybe text)

parseActors :: Cursor -> [Actor]
parseActors cur = trace "Parsing actors..." $
  mapMaybe parseActor (cur $// element (Name "actor" Nothing Nothing))

parseActor :: Cursor -> Maybe Actor
parseActor cur = do
  name <- listToMaybe $ cur $// element (Name "name" Nothing Nothing) &// content
  role <- listToMaybe $ cur $// element (Name "role" Nothing Nothing) &// content
  return Actor
    { name = T.unpack name
    , role = T.unpack role
    }

parseFileInfo :: Cursor -> Maybe FileInfo
parseFileInfo cur = do
  trace "Parsing streamDetails..." $ return ()
  streamDetails <- parseStreamDetails =<< listToMaybe (cur $// element (Name "streamdetails" Nothing Nothing))
  return FileInfo
    { streamDetails = streamDetails
    }

parseStreamDetails :: Cursor -> Maybe StreamDetails
parseStreamDetails cur = do
  trace "Parsing video..." $ return ()
  video <- parseVideo =<< listToMaybe (cur $// element (Name "video" Nothing Nothing))
  trace "Parsing audios..." $ return ()
  let audios = mapMaybe parseAudio (cur $// element (Name "audio" Nothing Nothing))
  trace "Parsing subtitles..." $ return ()
  let subtitles = mapMaybe parseSubtitle (cur $// element (Name "subtitle" Nothing Nothing))
  return StreamDetails
    { video = video
    , audios = audios
    , subtitles = subtitles
    }

parseVideo :: Cursor -> Maybe Video
parseVideo cur = trace "Parsing Video details..." $ Just Video
  { v_codec = traceShowId $ getElemText cur (Name "codec" Nothing Nothing)
  , aspect = traceShowId $ readElemText cur (Name "aspect" Nothing Nothing) 0.0
  , width = traceShowId $ readElemText cur (Name "width" Nothing Nothing) 0
  , height = traceShowId $ readElemText cur (Name "height" Nothing Nothing) 0
  }
  where
    getElemText cur name = T.unpack $ T.concat $ cur $// element name &// content
    readElemText cur name def = fromMaybe def . readMaybe . getElemText cur $ name

parseAudio :: Cursor -> Maybe Audio
parseAudio cur = trace "Parsing Audio details..." $ Just Audio
  { a_codec = traceShowId $ getElemText cur (Name "codec" Nothing Nothing)
  , a_language = traceShowId $ getElemText cur (Name "language" Nothing Nothing)
  , channels = traceShowId $ readElemText cur (Name "channels" Nothing Nothing) 0
  }
  where
    getElemText cur name = T.unpack $ T.concat $ cur $// element name &// content
    readElemText cur name def = fromMaybe def . readMaybe . getElemText cur $ name

parseSubtitle :: Cursor -> Maybe Subtitle
parseSubtitle cur = trace "Parsing Subtitle details..." $ Just Subtitle
  { s_language = traceShowId $ getElemText cur (Name "language" Nothing Nothing)
  }
  where
    getElemText cur name = T.unpack $ T.concat $ cur $// element name &// content
