{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Trakt.Client where

import Control.Monad.Reader
import Network.HTTP.Conduit

import Data.Aeson(FromJSON(..), decode)
import Data.List (intercalate)
import Data.List.Split (splitOn)

import Trakt.Models (ApiKey)

newtype TraktCall r = TraktCall { getData :: ReaderT ApiKey IO r }
    deriving (Monad, MonadIO, Functor, MonadReader ApiKey)

replace :: [Char] -> [Char] -> [Char] -> [Char]
replace old new = intercalate new . splitOn old

fetch :: FromJSON r => String -> TraktCall (Maybe r)
fetch url = do
    key <- ask
    let urlWithKey = replace "__apikey__" key url
    liftIO $ downloadContent urlWithKey

downloadContent :: FromJSON r => String -> IO(Maybe r)
downloadContent url = do
    result <- simpleHttp url
    return $ decode result

runTraktCall :: FromJSON r => TraktCall r -> ApiKey -> IO r
runTraktCall call key = runReaderT (getData call) key
