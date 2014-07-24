module Trakt.Models where

import qualified Data.Text.Lazy as TL

import Data.Time(UTCTime)

type ApiKey = String

data TraktEpisode = TraktEpisode {
        traktEpisodeSeason :: Int
    ,   traktEpisodeNumber :: Int
    ,   traktEpisodeOverview :: Maybe TL.Text
    ,   traktEpisodeFirstAired :: Maybe UTCTime
    ,   traktEpisodeScreenshot :: Maybe TL.Text
}