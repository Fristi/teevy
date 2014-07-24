module Teevy.Database.Models where

import Data.Time.Clock
import Data.Text.Lazy

------------------
-- Type synonyms -
------------------

-- USER ADMINISTRATION
type UserSessionToken = Text
type RemoteAddr = Text
type SessionLimit = Int

-- USER 
type UserId = Int
type Email = Text
type Password = Text
type Nickname = Text
type FacebookUserId = Text

-- EPISODE ID
type ShowId = Int
type SeasonNr = Int
type EpisodeNr = Int

-- SHOW
type ShowTitle = Text
type ShowPosterPath = Text
type ShowSearchTerm = Text
type ShowStartDate = UTCTime
type ShowEndDate = UTCTime


-------------
-- Entities -
-------------

data TvUser = TvUser {
    userId :: UserId,
    userNickname :: !Nickname,
    userEmail :: !Email,
    userPassword :: Maybe Password,
    oldPassword :: Maybe Text,
    oldSalt :: Maybe Text,
    facebookUserId :: Maybe FacebookUserId
}

data TvShow = TvShow {
    showId :: ShowId,
    showTitle :: !ShowTitle,
    showPosterPath :: Maybe ShowPosterPath,
    showInProduction :: Bool,
    showStarted :: Maybe ShowStartDate,
    showEnded :: Maybe ShowEndDate
}

data TvEpisodeId = TvEpisodeId {
    episodeIdSeasonNr :: SeasonNr,
    episodeIdEpisodeNr :: EpisodeNr,
    episodeIdShowId :: ShowId
} deriving (Show)

data TvEpisode = TvEpisode {
    episodeId :: TvEpisodeId,
    episodeTitle :: !Text,
    episodeReleaseDate :: Maybe UTCTime
}

data TvSubscription = TvSubscription {
    subscriptionEpisode :: Maybe TvEpisode,
    subscriptionShow :: TvShow
}

--------------------------
-- Data Transfer Objects -
--------------------------

data TvSubscriptionStatus = TvSubscriptionStatus {
    isSubscribed :: Bool,
    subscribedTo :: TvShow
}

data TvEpisodePointer = TvEpisodePointer {
    episodePointerEpisode :: TvEpisode,
    episodePointerShow :: TvShow
}

data TvShowSubscriptionStats = TvShowSubscriptionStats {
    showStatsShowId :: ShowId,
    showStatsShowTitle :: ShowTitle,
    showStatsNumberOfSubscriptions :: Int
}