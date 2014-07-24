{-# LANGUAGE DeriveGeneric #-}
module Teevy.Messages.Responses.Models where

import GHC.Generics
import Teevy.Database

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

data TvInputError = TvInputError {
	formKey :: !TL.Text,
	formError :: !TL.Text
} deriving (Show, Eq, Generic)

data TvResponse r = TvSuccess r
                  | TvSystemError TL.Text
                  | TvInputErrors [TvInputError]
                  deriving (Eq, Show, Generic)

data NextEpisodePointer = NextEpisodePointer {
    nextPointerEpisode :: TvEpisode,
    nextPointerShow :: TvShow
}

data LastEpisodePointer = LastEpisodePointer {
    lastPointerEpisode :: TvEpisode,
    lastPointerShow :: TvShow
}

data ShowSearchResultItem = ShowSearchResultItem {
    searchResultShowId :: ShowId,
    searchResultShowTitle :: !TL.Text,
    searchResultShowPosterPath :: Maybe TL.Text,
    searchResultIsSubscribedTo :: Bool
}

data WebsiteStatsResponse = WebsiteStatsResponse {
    statsNumRegisteredUsers :: Int,
    statsNumSubscriptions :: Int,
    statsTopTenMostSubscribed :: [TvShowSubscriptionStats],
    statsNumShows :: Int
} deriving (Generic)

data LoginResponse = LoginResponse {
    token :: !T.Text
} deriving (Eq, Generic)

data UserInfoResponse = UserInfoResponse {
    userInfoNickName :: !TL.Text,
    userInfoAvatarUrl :: !TL.Text
} deriving (Generic)

data ExploreResponse = ExploreResponse {
    shows :: [TvSubscriptionStatus]
} deriving (Generic)

data SearchResponse = SearchResponse {
    foundShows :: [ShowSearchResultItem]
} deriving (Generic)

data EpisodeListResponse = EpisodeListResponse {
    episodes :: [TvEpisode]
} deriving (Generic)

data SubscriptionsResponse = SubscriptionsResponse {
    subscriptionsUnconfigured :: [LastEpisodePointer],
    subscriptionsToWatch :: [NextEpisodePointer],
    subscriptionsToAir :: [NextEpisodePointer],
    subscriptionsFinished :: [TvShow],
    subscriptionsUnfinished :: [TvShow]
} deriving (Generic)
