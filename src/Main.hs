{-# LANGUAGE OverloadedStrings #-}

import Options.Applicative
import Control.Exception (try, SomeException)
import System.Directory
import System.FilePath
import Control.Monad
import Data.List (sortOn)
import Data.Maybe (mapMaybe, isNothing)
import Data.Either (partitionEithers) -- Import partitionEithers from Data.Either
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
import Prelude hiding (readFile) -- Hide Prelude's readFile to avoid ambiguity
import qualified Prelude (readFile) -- Qualify Prelude's readFile

data Options = Options
  { configFile :: FilePath
  }

parseOptions :: Parser Options
parseOptions = Options
  <$> strOption
      ( long "config"
     <> short 'c'
     <> metavar "CONFIG"
     <> help "Configuration file listing directories to scan for .nfo files" )

optsParser :: ParserInfo Options
optsParser = info (parseOptions <**> helper)
  ( fullDesc
 <> progDesc "Scan directories listed in a configuration file for .nfo files and convert them to JSON"
 <> header "parseMovie - A tool to convert movie .nfo files to JSON" )

main :: IO ()
main = do
  opts <- execParser optsParser
  processConfigFile (configFile opts)

processConfigFile :: FilePath -> IO ()
processConfigFile configPath = do
  dirs <- lines <$> Prelude.readFile configPath -- Use Prelude.readFile explicitly
  currentDir <- getCurrentDirectory
  let libraryDir = currentDir </> "dork_library"

  -- Ensure the dork_library directory exists
  createDirectoryIfMissing True libraryDir

  -- Initialize lists to store results from all directories
  allResults <- fmap concat . forM dirs $ \dir -> do
    processDirectory dir

  let (errors, movies) = partitionEithers allResults

  putStrLn "Processing complete."
  putStrLn $ "Total movies found: " ++ show (length movies)
  putStrLn $ "Total folders missing .nfo files: " ++ show (length errors)

  let sortedMovies = sortOn title movies

  -- Debugging: print the number of sorted movies
  putStrLn $ "Writing " ++ show (length sortedMovies) ++ " movies to library.json"

  runConduitRes $
    yieldMany sortedMovies
    .| mapC (\m -> let encoded = encode m in BS.concat (BL.toChunks encoded) `BS8.append` BS8.singleton '\n')
    .| sinkFileCautious (libraryDir </> "library.json")

  putStrLn $ "Library JSON written to: " ++ (libraryDir </> "library.json")

  runConduitRes $
    yieldMany errors
    .| mapC (\f -> let encoded = encode f in BS.concat (BL.toChunks encoded) `BS8.append` BS8.singleton '\n')
    .| sinkFileCautious (libraryDir </> "unNamed.json")

  putStrLn $ "Missing NFO JSON written to: " ++ (libraryDir </> "unNamed.json")

processDirectory :: FilePath -> IO [Either String Movie]
processDirectory dir = do
  folders <- listDirectory dir
  forM folders $ \folder -> do
    let folderPath = dir </> folder
    isDir <- doesDirectoryExist folderPath
    if isDir
      then do
        nfoFiles <- listDirectory folderPath
        let nfoFile = filter ((== ".nfo") . takeExtension) nfoFiles
        if null nfoFile
          then do
            putStrLn $ "No .nfo file in folder: " ++ folderPath
            return (Left folderPath)
          else do
            movieResult <- processNfoFile (folderPath </> head nfoFile)
            return movieResult
      else return (Left folderPath)

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
