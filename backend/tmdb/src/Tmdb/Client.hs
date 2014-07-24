{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Tmdb.Client where

import Control.Monad.Reader
import Network.HTTP.Conduit

import Data.Aeson(FromJSON(..), decode)
import Data.List (intercalate)
import Data.Maybe (catMaybes)

import Tmdb.Models (ApiKey)

import qualified Codec.Binary.Url as UE
import qualified Codec.Binary.UTF8.String as SE

import Control.Concurrent.Async (mapConcurrently)

newtype TmdbCall r = TmdbCall { getData :: ReaderT ApiKey IO r } 
    deriving (Monad, MonadIO, Functor, MonadReader ApiKey)

urlEncode :: String -> String
urlEncode = UE.encode . SE.encode

toQueryString :: [(String,String)] -> String
toQueryString = intercalate "&" . map paramTrans
    where
        paramTrans (k, v) = k ++ "=" ++ urlEncode(v)

fetchConcurrent :: FromJSON r => [String] -> Int -> TmdbCall [r]
fetchConcurrent urls nc = do
    key <- ask
    results <- liftIO $ loop key urls []
    return $ catMaybes results
    where
        loop _ [] acc = return acc
        loop key queue acc = do
            results <- mapConcurrently (downloadContent key []) (take nc queue)
            loop key (drop nc queue) (acc ++ results)

fetch :: FromJSON r => String -> [(String, String)] -> TmdbCall (Maybe r)
fetch url qs = do
    key <- ask
    liftIO $ downloadContent key qs url

downloadContent :: FromJSON r => String -> [(String, String)] -> String -> IO(Maybe r)
downloadContent key qs url = do
    result <- simpleHttp (url ++ "?" ++ qss)
    return $ decode result
    where
        qss = toQueryString $ ("api_key", key) : qs

runTmdbCall :: FromJSON r => TmdbCall r -> ApiKey -> IO r
runTmdbCall call key = runReaderT (getData call) key
