{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Teevy.Services.ShowService where

import qualified Teevy.Database.Show as DBS

import Teevy.Messages
import Teevy.Services.ApiCall

getEpisodes :: Int -> TvQueryHandler EpisodeListResponse
getEpisodes showId = do
    episodes <- tvQuery $ DBS.getEpisodes showId
    return $ TvSuccess $ EpisodeListResponse episodes