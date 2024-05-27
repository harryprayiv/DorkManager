{-# LANGUAGE OverloadedStrings #-}

import Options.Applicative
import Control.Exception (try, SomeException)
import System.Directory
import System.FilePath
import Control.Monad
import Data.List (sortOn, groupBy, (\\))
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Data.Either (partitionEithers)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Conduit (yieldMany, mapC, (.|), runConduitRes)
import Data.Conduit.Binary (sinkFileCautious)
import Text.XML (Document, readFile, def)
import Data.Aeson (encode, decode)
import Movies
import NFO
import JSON
import Control.Monad.Trans.Resource (MonadThrow)
import Control.Monad.IO.Class (liftIO)
import Prelude hiding (readFile)
import qualified Prelude (readFile)

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
  findAndProcessDuplicates

processConfigFile :: FilePath -> IO ()
processConfigFile configPath = do
  dirs <- lines <$> Prelude.readFile configPath
  currentDir <- getCurrentDirectory
  let libraryDir = currentDir </> "dork_library"

  createDirectoryIfMissing True libraryDir

  allResults <- fmap concat . forM dirs $ \dir -> do
    processDirectory dir

  let (errors, movies) = partitionEithers allResults

  putStrLn "Processing complete."
  putStrLn $ "Total movies found: " ++ show (length movies)
  putStrLn $ "Total folders missing .nfo files: " ++ show (length errors)

  let sortedMovies = sortOn title movies

  putStrLn $ "Writing " ++ show (length sortedMovies) ++ " movies to library.ndjson"

  runConduitRes $
    yieldMany sortedMovies
    .| mapC (\m -> let encoded = encode m in BS.concat (BL.toChunks encoded) `BS8.append` BS8.singleton '\n')
    .| sinkFileCautious (libraryDir </> "library.ndjson")

  putStrLn $ "Library JSON written to: " ++ (libraryDir </> "library.ndjson")

  runConduitRes $
    yieldMany errors
    .| mapC (\f -> let encoded = encode f in BS.concat (BL.toChunks encoded) `BS8.append` BS8.singleton '\n')
    .| sinkFileCautious (libraryDir </> "unNamed.ndjson")

  putStrLn $ "Missing NFO JSON written to: " ++ (libraryDir </> "unNamed.ndjson")

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
  let dir = takeDirectory filePath
  case result of
    Left ex -> do
      putStrLn $ "Error reading file: " ++ filePath ++ " - " ++ show ex
      return $ Left filePath
    Right doc -> do
      movie <- parseNfoSafe dir doc
      case movie of
        Just m -> return $ Right m
        Nothing -> do
          putStrLn $ "Failed to parse NFO file: " ++ filePath
          return $ Left filePath

parseNfoSafe :: FilePath -> Document -> IO (Maybe Movie)
parseNfoSafe dir doc = parseNfo dir doc

findAndProcessDuplicates :: IO ()
findAndProcessDuplicates = do
  currentDir <- getCurrentDirectory
  let libraryPath = currentDir </> "dork_library" </> "library.ndjson"
  movieLines <- BS8.lines <$> BS.readFile libraryPath
  let movies = mapMaybe (decode . BL.fromStrict) movieLines :: [Movie]
  let groupedMovies = groupBy ((==) `on` movieId) $ sortOn movieId movies
  let duplicateGroups = filter (\g -> length g > 1) groupedMovies
  let updatedMovies = concatMap updateGroup duplicateGroups ++ (movies \\ concat duplicateGroups)
  
  putStrLn $ "Updating and writing duplicates to library.ndjson"
  runConduitRes $
    yieldMany updatedMovies
    .| mapC (\m -> let encoded = encode m in BS.concat (BL.toChunks encoded) `BS8.append` BS8.singleton '\n')
    .| sinkFileCautious libraryPath

updateGroup :: [Movie] -> [Movie]
updateGroup group =
  let baseTitle = title (head group)
      updateMovie movie =
        movie { set = if null (set movie) then baseTitle else set movie }
  in map updateMovie group
