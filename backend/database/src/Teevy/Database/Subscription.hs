{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Teevy.Database.Subscription where

import Database.PostgreSQL.Simple

import Teevy.Database.Query
import Teevy.Database.Instances()
import Teevy.Database.Models

import GHC.Int

numSubscriptions :: TvQuery (Maybe Int)
numSubscriptions = queryMaybe "select count(0) from subscriptions" ()

topTenSubscriptions :: TvQuery [TvShowSubscriptionStats]
topTenSubscriptions = queryMany "select show_id, (select name from shows where id = show_id) as name, count(show_id) as show_count from subscriptions group by show_id order by show_count desc limit 10" ()

userWatchedAll :: UserId -> TvEpisodeId -> TvQuery Int64
userWatchedAll uid TvEpisodeId{..} = executeQuery "update subscriptions set episode_id.show_id = ?, episode_id.season_nr = ?, episode_id.episode_nr = ?, watched_all = true where user_id = ? and show_id = ?" (episodeIdShowId, episodeIdSeasonNr, episodeIdEpisodeNr, uid, episodeIdShowId)

markWatchedAllFalse :: ShowId -> TvQuery Int64
markWatchedAllFalse sid = executeQuery "update subscriptions set watched_all = false where (episode_id).show_id = ?" (Only sid)

setProgressionForUser :: UserId -> TvEpisodeId -> TvQuery Int64
setProgressionForUser uid TvEpisodeId{..} = executeQuery "update subscriptions set episode_id.show_id = ?, episode_id.season_nr = ?, episode_id.episode_nr = ?, watched_all = false where user_id = ? and show_id = ?" (episodeIdShowId, episodeIdSeasonNr, episodeIdEpisodeNr, uid, episodeIdShowId)

exploreShowsForUser :: UserId -> TvQuery [TvSubscriptionStatus]
exploreShowsForUser uid = queryMany "select uss.user_id is not null as subscribed, s.id, s.name, s.poster_path, s.in_production, s.show_started, s.show_ended from shows s left join subscriptions uss on s.id = uss.show_id and uss.user_id = ? order by s.name asc" (Only uid)

showsForUser :: UserId -> [ShowId] -> TvQuery [TvSubscriptionStatus]
showsForUser uid showIds = queryMany "select uss.user_id is not null as subscribed, s.id, s.name, s.poster_path, s.in_production, s.show_started, s.show_ended from shows s left join subscriptions uss on s.id = uss.show_id and uss.user_id = ? where s.id in ?" (uid, In(showIds))

subscribeShow :: ShowId -> UserId -> TvQuery Int64
subscribeShow sid uid = executeQuery "insert into subscriptions (user_id, show_id, watched_all) values (?,?, false)" (uid, sid)

unsubscribeShow :: ShowId -> UserId -> TvQuery Int64
unsubscribeShow sid uid = executeQuery "delete from subscriptions where user_id = ? and show_id = ?" (uid, sid)

toWatchShows :: UserId -> TvQuery [TvEpisodePointer]
toWatchShows uid = queryMany "select (e.id).season_nr, (e.id).episode_nr, (e.id).show_id, e.title, e.release_date, s.id, s.name, s.poster_path, s.in_production, s.show_started, s.show_ended from episodes e inner join shows s on (e.id).show_id = s.id where e.id in (select uss.episode_id from subscriptions as uss where uss.user_id = ? and uss.watched_all = false) and e.release_date is not null and e.release_date <= now() order by e.release_date desc" (Only uid)

toAirShows :: UserId -> TvQuery [TvEpisodePointer]
toAirShows uid = queryMany "select (e.id).season_nr, (e.id).episode_nr, (e.id).show_id, e.title, e.release_date, s.id, s.name, s.poster_path, s.in_production, s.show_started, s.show_ended from episodes e inner join shows s on (e.id).show_id = s.id where (e.release_date > now() or e.release_date is null) and e.id in (select uss.episode_id from subscriptions as uss where uss.user_id = ? and uss.watched_all = false) order by e.release_date asc nulls last" (Only uid)

finishedShows :: UserId -> TvQuery [TvShow]
finishedShows uid = queryMany "select s.id, s.name, s.poster_path, s.in_production, s.show_started, s.show_ended from subscriptions as uss inner join shows s on uss.show_id = s.id where uss.watched_all = true and s.in_production = false and uss.user_id = ?" (Only uid)

unfinishedShows :: UserId -> TvQuery [TvShow]
unfinishedShows uid = queryMany "select s.id, s.name, s.poster_path, s.in_production, s.show_started, s.show_ended from subscriptions uss inner join shows s on uss.show_id = s.id where uss.watched_all = true and s.in_production = true and uss.user_id = ?" (Only uid)

unconfiguredShows :: UserId -> TvQuery [TvEpisodePointer]
unconfiguredShows uid = queryMany "select (e.id).season_nr, (e.id).episode_nr, (e.id).show_id, e.title, e.release_date, s.id, s.name, s.poster_path, s.in_production, s.show_started, s.show_ended from subscriptions as uss inner join shows s on uss.show_id = s.id inner join episodes e on e.id = (select n.next_id from next_episodes n left join episodes en on n.next_id = en.id where (n.id).show_id = s.id and en.release_date <= now() order by (n.id).season_nr desc, (n.id).episode_nr desc limit 1) where uss.episode_id is null and uss.user_id = ?" (Only uid)