{-# LANGUAGE OverloadedStrings #-}

import Options.Applicative
import Control.Exception (try, SomeException)
import System.Directory
import System.FilePath
import Control.Monad
import Data.List (sortOn)
import Data.Maybe (mapMaybe, isNothing)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Conduit (yieldMany, mapC, (.|), runConduitRes)
import Data.Conduit.Binary (sinkFileCautious)
import Text.XML (Document, readFile, def)
import Data.Aeson (encode)
import Movies
import NFO
import JSON
import Control.Monad.Trans.Resource (MonadThrow)
import Control.Monad.IO.Class (liftIO)

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
            movieResult <- processNfoFile (folderPath </> head nfoFile)
            case movieResult of
              Left err -> return (folderPath, Nothing)
              Right movie -> return (folderPath, Just movie)
      else return (folderPath, Nothing)

  let movies = mapMaybe snd allMovies
  let missingNfoFiles = map fst $ filter (isNothing . snd) allMovies

  putStrLn "Processing complete."
  putStrLn $ "Total folders scanned: " ++ show (length folders)
  putStrLn $ "Movies found: " ++ show (length movies)
  putStrLn $ "Folders missing .nfo files: " ++ show (length missingNfoFiles)

  forM_ movies $ \movie -> putStrLn $ "Found movie: " ++ title movie

  let sortedMovies = sortOn title movies

  -- Debugging: print the number of sorted movies
  putStrLn $ "Writing " ++ show (length sortedMovies) ++ " movies to library.json"

  runConduitRes $
    yieldMany sortedMovies
    .| mapC (\m -> let encoded = encode m in BS.concat (BL.toChunks encoded) `BS8.append` BS8.singleton '\n')
    .| sinkFileCautious (dir </> "library.json")

  putStrLn $ "Library JSON written to: " ++ (dir </> "library.json")

  runConduitRes $
    yieldMany missingNfoFiles
    .| mapC (\f -> let encoded = encode f in BS.concat (BL.toChunks encoded) `BS8.append` BS8.singleton '\n')
    .| sinkFileCautious (dir </> "unNamed.json")

  putStrLn $ "Missing NFO JSON written to: " ++ (dir </> "unNamed.json")

processNfoFile :: FilePath -> IO (Either String Movie)
processNfoFile filePath = do
  putStrLn $ "Processing file: " ++ filePath
  result <- try (Text.XML.readFile def filePath) :: IO (Either SomeException Document)
  case result of
    Left ex -> do
      putStrLn $ "Error reading file: " ++ filePath ++ " - " ++ show ex
      return $ Left filePath
    Right doc -> 
      case parseNfoSafe doc of
        Just movie -> return $ Right movie
        Nothing -> do
          putStrLn $ "Failed to parse NFO file: " ++ filePath
          return $ Left filePath

parseNfoSafe :: Document -> Maybe Movie
parseNfoSafe doc = 
  case parseNfo doc of
    Just movie -> Just movie
    Nothing -> Nothing
