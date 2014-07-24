{-# LANGUAGE RecordWildCards #-}
module Teevy.Database.Instances where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField

import Control.Applicative
import Control.Monad (liftM3)
import Data.Function (on)

import Teevy.Database.Models

-- Equality

instance Eq TvShow where
    (==) = (==) `on` showId

instance Eq TvEpisode where
    (==) = (==) `on` episodeId

instance Eq TvEpisodeId where
    (==) a b = all (\f -> f b) [seasonNrEq a, episodeNrEq a, showIdEq a]
        where
            seasonNrEq = (==) `on` episodeIdSeasonNr
            episodeNrEq = (==) `on` episodeIdEpisodeNr
            showIdEq = (==) `on` episodeIdShowId

-- Ordering

instance Ord TvShow where
    compare = compare `on` showId

instance Ord TvEpisode where
    compare = compare `on` episodeId

instance Ord TvEpisodeId where
    compare = compare `on` episodeIdShowId

-- To Row

instance ToRow TvEpisodeId where
    toRow episodeId = [toField (episodeIdSeasonNr episodeId), toField (episodeIdEpisodeNr episodeId), toField (episodeIdShowId episodeId)]

instance ToRow TvEpisode where
    toRow ep = toRow (episodeId ep) ++ [toField (episodeTitle ep), toField (episodeReleaseDate ep)]

-- From Row

instance FromRow TvShowSubscriptionStats where
    fromRow = TvShowSubscriptionStats <$> field <*> field <*> field

instance FromRow TvEpisodeId where
    fromRow = TvEpisodeId <$> field <*> field <*> field

instance FromRow TvEpisode where
    fromRow = TvEpisode <$> (liftM3 TvEpisodeId field field field) <*> field <*> field

instance FromRow TvShow where
    fromRow = TvShow <$> field <*> field <*> field <*> field <*> field <*> field

instance FromRow TvSubscriptionStatus where
    fromRow = TvSubscriptionStatus <$> field <*> liftM6 TvShow field field field field field field

instance FromRow TvEpisodePointer where
    fromRow = TvEpisodePointer <$> 
                liftM3 TvEpisode (liftM3 TvEpisodeId field field field) field field <*> 
                liftM6 TvShow field field field field field field

instance FromRow TvUser where
    fromRow = TvUser <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow Int where
    fromRow = field

liftM6  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m r
liftM6 f m1 m2 m3 m4 m5 m6 = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6; return (f x1 x2 x3 x4 x5 x6) }