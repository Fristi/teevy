{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Teevy.Messages.Responses.Instances where

import Data.Aeson

import Teevy.Database
import Teevy.Messages.Responses.Models


-- Derived Generic
instance ToJSON WebsiteStatsResponse
instance ToJSON LoginResponse
instance ToJSON UserInfoResponse
instance ToJSON EpisodeListResponse
instance ToJSON SubscriptionsResponse
instance ToJSON ExploreResponse
instance ToJSON SearchResponse

-- Custom

instance ToJSON NextEpisodePointer where
    toJSON NextEpisodePointer{..} = object ["nextEpisode" .= nextPointerEpisode, "show" .= nextPointerShow]

instance ToJSON LastEpisodePointer where
    toJSON LastEpisodePointer{..} = object ["lastEpisode" .= lastPointerEpisode, "show" .= lastPointerShow]

instance ToJSON ShowSearchResultItem where
    toJSON ShowSearchResultItem{..} = object ["showId" .= searchResultShowId, "showTitle" .= searchResultShowTitle, "showPosterPath" .= searchResultShowPosterPath, "isSubscribed" .= searchResultIsSubscribedTo]

-- Database instances

instance ToJSON TvShow where
    toJSON TvShow{..} = object ["showId" .= showId, "showTitle" .= showTitle, "showPosterPath" .= showPosterPath, "showInProduction" .= showInProduction, "showStarted" .= showStarted, "showEnded" .= showEnded]

instance ToJSON TvEpisodeId where
    toJSON TvEpisodeId{..} = object ["showId" .= episodeIdShowId, "seasonNr" .= episodeIdSeasonNr, "episodeNr" .= episodeIdEpisodeNr]

instance ToJSON TvEpisode where
    toJSON TvEpisode{..} = object ["episodeId" .= episodeId, "episodeTitle" .= episodeTitle, "episodeReleaseDate" .= episodeReleaseDate]

instance ToJSON TvSubscriptionStatus where
    toJSON TvSubscriptionStatus{..} = object ["subscribedTo" .= subscribedTo, "isSubscribed" .= isSubscribed]

instance ToJSON TvShowSubscriptionStats where
    toJSON TvShowSubscriptionStats{..} = object ["showId" .= showStatsShowId, "showTitle" .= showStatsShowTitle, "numberOfSubscriptions" .= showStatsNumberOfSubscriptions]

-- Teevy types

instance ToJSON TvInputError
instance ToJSON r => ToJSON (TvResponse r) where
    toJSON (TvSuccess x) = object ["status" .= ("success" :: String), "data" .= x]
    toJSON (TvSystemError msg) = object ["status" .= ("error" :: String), "message" .= msg]
    toJSON (TvInputErrors e) = object ["status" .= ("fail" :: String), "data" .= e]