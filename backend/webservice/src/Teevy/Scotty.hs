{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Teevy.Scotty (exceptionHandler, notFoundHandler, doCommand, doQuery) where

import Control.Monad.Trans (liftIO)

import Data.Aeson (FromJSON(..), ToJSON(..))

import Web.Scotty
import Network.HTTP.Types.Status (status500, status404)

import Teevy.Messages
import Teevy.Services.ApiCall

import Teevy.Shared.Log
import Teevy.Shared.Config (TvConfig(..))

import Data.Pool
import qualified Database.PostgreSQL.Simple as P

exceptionHandler :: Show s => s -> ActionM ()
exceptionHandler errmsg = do
    status status500
    liftIO $ err "teevy" (show errmsg)
    let msg = TvSystemError "Something went wrong, try again later" :: TvResponse ()
    json msg

notFoundHandler :: ActionM ()
notFoundHandler = do
    status status404
    let msg = TvSystemError "Not found" :: TvResponse ()
    json msg

handleTvApiCall :: (ToJSON r) => TvApiCall r -> TvConfig -> Pool P.Connection -> ActionM()
handleTvApiCall call cfg pool = do
    auth <- header "Authorization"
    addr <- header "X-Real-IP"
    res <- liftIO $ runTvApiCall call cfg pool auth addr
    json res

doQuery :: (ToJSON o) => TvConfig -> Pool P.Connection -> TvQueryHandler o -> ActionM ()
doQuery cfg pool handler = handleTvApiCall handler cfg pool

doCommand :: (FromJSON i, ToJSON o) => TvConfig -> Pool P.Connection -> TvCommandHandler i o -> ActionM ()
doCommand cfg pool handler = do
    command <- jsonData
    handleTvApiCall (handler command) cfg pool