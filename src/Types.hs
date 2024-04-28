{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# HLINT ignore "Redundant id" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Types where

import Control.Applicative ((<|>))
import Control.Monad (filterM)
import Data.Aeson (
    FromJSON (..),
    Result (Success),
    ToJSON (..),
    Value,
    decode,
    eitherDecodeStrict,
    fromJSON,
    object,
    withObject,
    (.!=),
    (.:),
    (.:?),
    (.=),
 )

import Data.Aeson.Types (Parser, Result (..), withScientific, withText)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (readFile)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
-- import Data.Scientific (toBoundedInteger)
import qualified Data.Text as T
import qualified Data.Vector as V
