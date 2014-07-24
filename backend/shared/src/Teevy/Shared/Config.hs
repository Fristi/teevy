{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Teevy.Shared.Config (loadConfig, TvConfig(..)) where

import Control.Exception
import Control.Applicative

import Data.Configurator
import Data.Configurator.Types

import System.Directory (doesFileExist)

import Database.PostgreSQL.Simple (ConnectInfo(..))

import Prelude hiding (lookup)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B

data TvConfig = TvConfig {
        teevyConnectInfo :: ConnectInfo
    ,   teevyPort :: Int
    ,   teevySecret :: T.Text
    ,   tmdbApiKey :: TL.Text
    ,   facebookSecret :: B.ByteString
} deriving (Show, Eq)

withLoad :: [Worth FilePath] -> (Config -> IO(Maybe a)) -> IO (Maybe a)
withLoad c t = do
    mb <- try $ load c
    case mb of
        Left (err :: SomeException) -> do
          putStr $ show err -- TODO: log this nicely
          return Nothing
        Right cfg -> t cfg

processDatabaseInfo :: Config -> IO (Maybe ConnectInfo)
processDatabaseInfo cfg = do
  host <- lookup cfg "db.host"
  port <- lookup cfg "db.port"
  db <- lookup cfg "db.db"
  user <- lookup cfg "db.user"
  pass <- lookup cfg "db.pass"
  return (ConnectInfo <$> host <*> port <*> user <*> pass <*> db)

processConfig :: Config -> IO (Maybe TvConfig)
processConfig cfg = do
  db <- processDatabaseInfo cfg
  port <- lookup cfg "teevy.port"
  secret <- lookup cfg "teevy.secret"
  tmdbKey <- lookup cfg "tmdb.key"
  fbSecret <- lookup cfg "facebook.secret"
  return (TvConfig <$> db <*> port <*> secret <*> tmdbKey <*> fbSecret)

loadConfig :: String -> IO (Maybe TvConfig)
loadConfig path = do
  exists <- doesFileExist path
  if exists
  then withLoad [Required path] processConfig
  else return Nothing