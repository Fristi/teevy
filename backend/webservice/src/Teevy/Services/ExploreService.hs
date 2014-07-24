{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Teevy.Services.ExploreService where

import Teevy.Database.Subscription
import Teevy.Database

import Teevy.Messages
import Teevy.Services.ApiCall

import Data.Maybe (isJust)

import qualified Tmdb as Tmdb
import qualified Data.Text.Lazy as TL

fromTmdbSearchResultToSearchItem :: Tmdb.TmdbSearchResult -> ShowSearchResultItem
fromTmdbSearchResultToSearchItem res = ShowSearchResultItem (Tmdb.searchResultShowId res) (Tmdb.searchResultShowName res) (Tmdb.searchResultPosterPath res) False

fromSubStatusToSearchItem :: TvSubscriptionStatus -> ShowSearchResultItem
fromSubStatusToSearchItem res = ShowSearchResultItem 
    ((showId . subscribedTo) $ res) 
    ((showTitle . subscribedTo) $ res) 
    ((showPosterPath . subscribedTo) $ res) 
    (isSubscribed res)

exceptShows :: [ShowId] -> [Tmdb.TmdbSearchResult] -> [Tmdb.TmdbSearchResult]
exceptShows showIds = filter (not . filterShow)
    where
        filterShow shw = any (comparator shw) showIds
        comparator shw showId = (Tmdb.searchResultShowId shw) == showId

filterShowsWithoutPosterPath :: [Tmdb.TmdbSearchResult] -> [Tmdb.TmdbSearchResult]
filterShowsWithoutPosterPath = filter (hasPosterPath)
    where
        hasPosterPath shw = isJust (Tmdb.searchResultPosterPath shw)

search :: TvCommandHandler SearchShowCommand SearchResponse
search cmd = withUser doTheMovieDbSearch    
    where
        searchTermStr = TL.unpack . searchTerm $ cmd
        doTheMovieDbSearch userId = tmdbMaybe (Tmdb.search searchTermStr) (mapResultsToUserSubscriptions userId)
        mapResultsToUserSubscriptions userId resp = do
            let searchResults = Tmdb.results resp
            let ids = map (Tmdb.searchResultShowId) searchResults
            
            foundSubscriptionShows <- tvQuery $ showsForUser userId ids

            let foundShowsId = map (showId . subscribedTo) foundSubscriptionShows
            let filteredSearchResults = filterShowsWithoutPosterPath . (exceptShows foundShowsId) $ searchResults
            let unsubscribedItems = map fromTmdbSearchResultToSearchItem filteredSearchResults
            let databaseItems = map fromSubStatusToSearchItem foundSubscriptionShows

            return $ TvSuccess $ SearchResponse (databaseItems ++ unsubscribedItems)
                
explore :: TvQueryHandler ExploreResponse
explore = withUser doExplore
    where doExplore userId = do
            tvShows <- tvQuery $ exploreShowsForUser userId
            return $ TvSuccess $ ExploreResponse tvShows