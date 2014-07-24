{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Teevy.Services.StatService where

import Control.Applicative

import Teevy.Messages

import Teevy.Database.User
import Teevy.Database.Show
import Teevy.Database.Subscription

import Teevy.Services.ApiCall

getWebsiteStats :: TvQueryHandler WebsiteStatsResponse
getWebsiteStats = do
    nrShows <- tvQuery $ numShows
    nrUsers <- tvQuery $ numUsers
    nrSubscriptions <- tvQuery $ numSubscriptions
    showStats <- tvQuery $ topTenSubscriptions

    let resp = WebsiteStatsResponse <$> nrUsers <*> nrSubscriptions <*> pure showStats <*> nrShows

    case resp of
        Just(r) -> return $ TvSuccess r
        Nothing -> return $ TvSystemError "Couldn't fetch stats"