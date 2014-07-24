{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Teevy.Database.Show where

import Database.PostgreSQL.Simple

import Teevy.Database.Query
import Teevy.Database.Instances()
import Teevy.Database.Models
import Data.Maybe (isJust)
import Data.Time.Clock

import Control.Monad (when, void)

import GHC.Int

isShowAiringInFuture :: ShowId -> TvQuery Bool
isShowAiringInFuture sid = do
    res <- queryMaybe "select (select count(0) from episodes e where (e.id).show_id = ?) as numEpisodes, count(0) as futureAiringEpisodes from episodes e where (e.id).show_id = ? and (e.release_date >= now() or e.release_date is null)" (sid, sid) :: TvQuery (Maybe (Int, Int))
    case res of
        Just(numEpisodes, futureAiringEpisodes) -> return $ numEpisodes == futureAiringEpisodes
        Nothing -> return False

numShows :: TvQuery (Maybe Int)
numShows = queryMaybe "select count(0) from shows" ()

notifyEpisodesAreChanged :: TvQuery Int64
notifyEpisodesAreChanged = executeQuery "notify episodesMutated" ()

truncateNextEpisodesTable :: TvQuery Int64
truncateNextEpisodesTable = executeQuery "truncate next_episodes" ()

insertNextEpisodes :: TvQuery Int64
insertNextEpisodes = executeQuery "insert into next_episodes select episodes.id, lag(episodes.id) over (partition by (episodes.id).show_id order by (episodes.id).season_nr desc, (episodes.id).episode_nr desc) as next_id from episodes order by (episodes.id).show_id asc, (episodes.id).season_nr asc, (episodes.id).episode_nr asc" ()

updateEpisodesTable :: TvQuery Int64
updateEpisodesTable = truncateNextEpisodesTable >> insertNextEpisodes

getShowIds :: TvQuery [Int]
getShowIds = queryMany "select id from shows" ()

getShows :: TvQuery [TvShow]
getShows = queryMany "select id, name, poster_path, in_production, show_started, show_ended from shows" ()

getEpisodes :: ShowId -> TvQuery [TvEpisode]
getEpisodes showId = queryMany "select (id).season_nr, (id).episode_nr, (id).show_id, title, release_date from episodes where (id).show_id = ? order by (id).season_nr asc, (id).episode_nr asc;" (Only showId)

getNextEpisode :: TvEpisodeId -> TvQuery (Maybe TvEpisode)
getNextEpisode TvEpisodeId{..} = queryMaybe "select (e.id).season_nr, (e.id).episode_nr, (e.id).show_id, e.title, e.release_date from episodes e where e.id = (select next_id from next_episodes n where (n.id).show_id = ? and (n.id).season_nr = ? and (n.id).episode_nr = ?)" (episodeIdShowId, episodeIdSeasonNr, episodeIdEpisodeNr)

saveShow :: TvShow -> TvQuery ()
saveShow shw = do
    affected <- updateShowIfExists shw 
    when (affected == 0) $ (void . insertShowIfNotExist) shw

updateShowIfExists :: TvShow -> TvQuery Int64
updateShowIfExists TvShow{..} = executeQuery "update shows set name = ?, poster_path = ?, in_production = ?, show_started = ?, show_ended = ? where id = ?" (showTitle, showPosterPath, showInProduction, showStarted, showEnded, showId)

insertShowIfNotExist :: TvShow -> TvQuery Int64
insertShowIfNotExist TvShow{..} = executeQuery "insert into shows (id, name, poster_path, in_production, show_started, show_ended) select ?, ?, ?, ?, ?, ? where not exists (select 0 from shows where id = ?)" (showId, showTitle, showPosterPath, showInProduction, showStarted, showEnded, showId)

saveEpisode :: TvEpisode -> TvQuery ()
saveEpisode ep = do
    affected <- updateEpisodeIfExists ep
    when (affected == 0) $ (void . insertEpisodeIfNotExist) ep

updateEpisodeIfExists :: TvEpisode -> TvQuery Int64
updateEpisodeIfExists ep = executeQuery "update episodes e set title = ?, release_date = ? where (e.id).season_nr = ? and (e.id).episode_nr = ? and (e.id).show_id = ?" ((episodeTitle ep), (episodeReleaseDate ep), seasonNr, episodeNr, showId)
    where
        seasonNr = episodeIdSeasonNr . episodeId $ ep
        episodeNr = episodeIdEpisodeNr . episodeId $ ep
        showId = episodeIdShowId . episodeId $ ep

insertEpisodeIfNotExist :: TvEpisode -> TvQuery Int64
insertEpisodeIfNotExist ep = executeQuery "insert into episodes (id.season_nr, id.episode_nr, id.show_id, title, release_date) select ?, ?, ?, ?, ? where not exists (select 0 from episodes e where (e.id).season_nr = ? and (e.id).episode_nr = ? and (e.id).show_id = ?)" (seasonNr, episodeNr, showId, (episodeTitle ep), (episodeReleaseDate ep), seasonNr, episodeNr, showId)
    where
        seasonNr = episodeIdSeasonNr . episodeId $ ep
        episodeNr = episodeIdEpisodeNr . episodeId $ ep
        showId = episodeIdShowId . episodeId $ ep

getShow :: ShowId -> TvQuery (Maybe TvShow)
getShow showId = queryMaybe "select id, name, poster_path, in_production, show_started, show_ended from shows where id = ?" (Only showId)

isShowInDatabase :: ShowId -> TvQuery Bool
isShowInDatabase showId = (getShow showId) >>= return . isJust

--sid seasonNr Trakt.traktEpisodeNumber Trakt.traktEpisodeFirstAired

updateEpisodeInfo :: ShowId -> SeasonNr -> EpisodeNr -> Maybe UTCTime -> TvQuery Int64
updateEpisodeInfo sid seasonNr episodeNr airDate = executeQuery "update episodes set release_date = ? where (id).show_id = ? and (id).season_nr = ? and (id).episode_nr = ?" (airDate, sid, seasonNr, episodeNr)