{-# LANGUAGE OverloadedStrings #-}
module Trakt.Instances() where

import Data.Aeson ((.:), (.:?), FromJSON(..), Value(..))
import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM, mzero)

import Data.Time.ISO8601
import Data.Time.Clock (UTCTime)

import Trakt.Models

parseDate :: Maybe String -> Maybe UTCTime
parseDate str = str >>= parseISO8601

instance FromJSON TraktEpisode where
    parseJSON (Object v) =
        TraktEpisode <$>
            (v .: "season") <*>
            (v .: "episode") <*>
            (v .:? "overview") <*>
            liftM parseDate (v .:? "first_aired_iso") <*>
            (v .:? "screen")
            
    parseJSON _ = mzero