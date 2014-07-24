{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Teevy.Messages.Commands.Instances where

import Data.Aeson
import Control.Monad (mzero)
import Control.Applicative

import Teevy.Database
import Teevy.Messages.Commands.Models

instance FromJSON TvEpisodeId where
    parseJSON (Object v) = TvEpisodeId <$> (v .: "seasonNr") <*> (v .: "episodeNr") <*> (v .: "showId")
    parseJSON _ = mzero


instance FromJSON SearchShowCommand
instance FromJSON NextEpisodeCommand
instance FromJSON SetShowProgressionCommand
instance FromJSON UserRegisterCommand
instance FromJSON UserLoginCommand
instance FromJSON SubscribeShowCommand
instance FromJSON UnsubscribeShowCommand