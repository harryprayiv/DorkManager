{-# LANGUAGE OverloadedStrings #-}

import Options.Applicative
import System.Directory
import System.FilePath
import Control.Monad
import Data.List (sortOn)
import Data.Maybe (mapMaybe, isNothing)
import qualified Data.ByteString.Lazy as B
import Text.XML (readFile, def)
import Movies
import NFO
import JSON

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

optsParser :: ParserInfo Options
optsParser = info (parseOptions <**> helper)
  ( fullDesc
 <> progDesc "Scan a directory for .nfo files and convert them to JSON"
 <> header "parseMovie - A tool to convert movie .nfo files to JSON" )

main :: IO ()
main = do
  opts <- execParser optsParser
  processDirectory (directory opts)

processDirectory :: FilePath -> IO ()
processDirectory dir = do
  folders <- listDirectory dir
  allMovies <- forM folders $ \folder -> do
    let folderPath = dir </> folder
    isDir <- doesDirectoryExist folderPath
    if isDir
      then do
        nfoFiles <- listDirectory folderPath
        let nfoFile = filter ((== ".nfo") . takeExtension) nfoFiles
        if null nfoFile
          then do
            putStrLn $ "No .nfo file in folder: " ++ folderPath
            return (folderPath, Nothing)
          else do
            movie <- processNfoFile (folderPath </> head nfoFile)
            return (folderPath, Just movie)
      else return (folderPath, Nothing)

  let movies = mapMaybe snd allMovies
  let missingNfoFiles = map fst $ filter (isNothing . snd) allMovies

  putStrLn "Processing complete."
  putStrLn $ "Total folders scanned: " ++ show (length folders)
  putStrLn $ "Movies found: " ++ show (length movies)
  putStrLn $ "Folders missing .nfo files: " ++ show (length missingNfoFiles)

  forM_ movies $ \movie -> putStrLn $ "Found movie: " ++ title movie

  let sortedMovies = sortOn title movies
  writeJson (dir </> "library.json") sortedMovies
  putStrLn $ "Library JSON written to: " ++ (dir </> "library.json")
  writeJson (dir </> "unNamed.json") missingNfoFiles
  putStrLn $ "Missing NFO JSON written to: " ++ (dir </> "unNamed.json")

processNfoFile :: FilePath -> IO Movie
processNfoFile filePath = do
  putStrLn $ "Processing file: " ++ filePath
  doc <- Text.XML.readFile def filePath
  let movie = parseNfo doc
  return movie
