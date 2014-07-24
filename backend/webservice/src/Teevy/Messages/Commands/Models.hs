{-# LANGUAGE DeriveGeneric #-}
module Teevy.Messages.Commands.Models where

import GHC.Generics
import Teevy.Database
import qualified Data.Text.Lazy as TL

data SearchShowCommand = SearchShowCommand {
    searchTerm :: ShowSearchTerm
} deriving (Show, Generic)

data NextEpisodeCommand = NextEpisodeCommand {
    currentEpisodeId :: TvEpisodeId
} deriving (Show, Generic)

data SubscribeShowCommand = SubscribeShowCommand {
	subscribeShowId :: ShowId
} deriving (Show, Generic)

data SetShowProgressionCommand = SetShowProgressionCommand {
    progression :: TvEpisodeId
} deriving (Show, Generic)

data UnsubscribeShowCommand = UnsubscribeShowCommand {
	unsubscribeShowId :: ShowId
} deriving (Show, Generic)

-- Identity

data UserRegisterCommand = UserRegisterCommand {
	registerEmail :: !TL.Text,
	registerNickname :: !TL.Text,
	registerPassword :: !TL.Text
} deriving (Show, Generic)

data UserLoginCommand = UserLoginCommand {
	login :: !TL.Text,
	loginPassword :: !TL.Text
} deriving (Show, Generic)

