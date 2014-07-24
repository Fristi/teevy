{-# LANGUAGE OverloadedStrings #-}
module Tmdb.Instances() where

import Data.Aeson ((.:), (.:?), FromJSON(..), Value(..))
import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM)

import Data.Time.Format (parseTime)
import Data.Time.Clock (UTCTime)
import System.Locale (defaultTimeLocale)

import Tmdb.Models

parseDate :: Maybe String -> Maybe UTCTime
parseDate input = 
    case input of 
        Nothing -> Nothing
        Just(str) -> parseTime defaultTimeLocale format str
    where
        format = "%Y-%m-%d"


instance (FromJSON a) => FromJSON (TmdbResult a) where
    parseJSON (Object v) = 
        TmdbResult <$>
            (v .: "page") <*>
            (v .: "total_pages") <*>
            (v .: "total_results") <*>
            (v .: "results")

    parseJSON _ = error "Cannot parse json"

instance FromJSON TmdbSearchResult where
    parseJSON (Object v) =
        TmdbSearchResult <$>
            (v .: "id") <*>
            (v .: "name") <*>
            (v .:? "poster_path")

    parseJSON _ = error "Cannot parse json"

instance FromJSON TmdbTvShow where
    parseJSON (Object v) =
        TmdbTvShow <$>
            (v .: "id") <*>
            (v .: "name") <*>
            (v .: "in_production") <*>
            (v .: "seasons") <*>
            (v .:? "poster_path") <*>
            liftM parseDate (v .:? "first_air_date") <*>
            liftM parseDate (v .:? "last_air_date")
            
    parseJSON _ = error "Cannot parse json"

instance FromJSON TmbdTvShowSeasonInfo where
    parseJSON (Object v) = TmbdTvShowSeasonInfo <$> (v .: "season_number")
    
    parseJSON _ = error "Cannot parse jsons"

instance FromJSON TmdbShowSeason where
    parseJSON (Object v) =
        TmdbShowSeason <$> (v .: "episodes") <*> (v .: "season_number")

    parseJSON _ = error "Cannot parse json"


instance FromJSON TmdbShowEpisode where
    parseJSON (Object v) =
        TmdbShowEpisode <$>
            liftM parseDate (v .:? "air_date") <*>
            (v .:? "episode_number") <*>
            (v .: "name")
            
    parseJSON _ = error "Cannot parse json"