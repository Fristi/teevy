module Tmdb.Models where

import qualified Data.Text.Lazy as TL

import Data.Time(UTCTime)

type ApiKey = String
type SearchTerm = String
type ShowId = Int
type SeasonNr = Int

data TmdbResult a = TmdbResult {
    resultsCurrentPage :: Int,
    resultsTotalPages :: Int,
    resultsTotalResults :: Int,
    results :: [a]
} deriving (Show)

data TmdbSearchResult = TmdbSearchResult {
    searchResultShowId :: Int,
    searchResultShowName :: !TL.Text,
    searchResultPosterPath :: Maybe TL.Text
}  deriving (Show)

data TmdbShowSeason = TmdbShowSeason {
    showSeasonEpisodes :: [TmdbShowEpisode],
    showSeasonNumber :: Int
}  deriving (Show)

data TmdbShowEpisode = TmdbShowEpisode {
    episodeAirdate :: Maybe UTCTime,
    episodeNr :: Maybe Int,
    episodeTitle :: !TL.Text
}  deriving (Show)

data TmdbTvShow = TmdbTvShow {
    showId :: Int,
    showName :: TL.Text,
    showInProduction :: Bool,
    showSeasons :: [TmbdTvShowSeasonInfo],
    showPosterPath :: Maybe TL.Text,
    showFirstAirDate :: Maybe UTCTime,
    showLastAirDate :: Maybe UTCTime
} deriving (Show)

data TmbdTvShowSeasonInfo = TmbdTvShowSeasonInfo {
    seasonNumber :: Int
} deriving (Show)