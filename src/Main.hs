{-# LANGUAGE OverloadedStrings #-}

import Options.Applicative
import System.Directory
import System.FilePath
import Control.Monad
import Data.Maybe (mapMaybe)
import qualified Data.ByteString.Lazy as B
import Text.XML (readFile, def)
import Text.XML.Lens (toXMLDocument)

data Options = Options
  { directory :: FilePath
  }

parseOptions :: Parser Options
parseOptions = Options
  <$> strOption
      ( long "directory"
     <> short 'd'
     <> metavar "DIR"
     <> help "Directory to scan for .nfo files" )

main :: IO ()
main = do
  opts <- execParser optsParser
  processDirectory (directory opts)
  where
    optsParser = info (parseOptions <**> helper)
      ( fullDesc
     <> progDesc "Scan a directory for .nfo files and convert them to JSON"
     <> header "nfo2json - A tool to convert movie .nfo files to JSON" )

processDirectory :: FilePath -> IO ()
processDirectory dir = do
  files <- listDirectory dir
  let nfoFiles = map (dir </>) $ filter ((== ".nfo") . takeExtension) files
  movies <- mapM processNfoFile nfoFiles
  mapM_ writeJson movies
  where
    writeJson (filePath, movie) = B.writeFile (replaceExtension filePath "json") (movieToJSON movie)

processNfoFile :: FilePath -> IO (FilePath, Movie)
processNfoFile filePath = do
  doc <- Text.XML.readFile def filePath
  let movie = parseNfo doc
  return (filePath, movie)
