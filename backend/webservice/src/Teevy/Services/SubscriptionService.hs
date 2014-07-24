{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Teevy.Services.SubscriptionService where

import Teevy.Database
import Teevy.Database.Show
import Teevy.Database.Subscription
import Teevy.Messages
import Teevy.Services.ApiCall
import Teevy.Shared.TmdbConversion
import qualified Tmdb as Tmdb

subscriptions :: TvQueryHandler SubscriptionsResponse
subscriptions = withUser getSubscriptions
    where getSubscriptions userId = do
            
            unconfigured <- tvQuery $ unconfiguredShows userId
            toWatch <- tvQuery $ toWatchShows userId
            toAir <- tvQuery $ toAirShows userId
            finished <- tvQuery $ finishedShows userId
            unfinished <- tvQuery $ unfinishedShows userId

            return $ TvSuccess $ SubscriptionsResponse 
                ((map toLastEpisodePointer) unconfigured)
                ((map toNextEpisodePointer) toWatch)
                ((map toNextEpisodePointer) toAir)
                finished
                unfinished

setProgression :: TvCommandHandler SetShowProgressionCommand SubscriptionsResponse
setProgression SetShowProgressionCommand{..} = withUser doSetProgression
    where doSetProgression userId = do
            _ <- tvQuery $ setProgressionForUser userId progression
            subscriptions    

nextEpisode :: TvCommandHandler NextEpisodeCommand SubscriptionsResponse
nextEpisode NextEpisodeCommand{..} = withUser doNextEpsiode
    where doNextEpsiode userId = do
            nextEp <- tvQuery $ getNextEpisode currentEpisodeId
            case nextEp of
                Nothing -> do
                    _ <- tvQuery $ userWatchedAll userId currentEpisodeId
                    subscriptions
                Just ep -> do
                    _ <- tvQuery $ setProgressionForUser userId (episodeId ep)
                    subscriptions         

unsubscribe :: TvCommandHandler UnsubscribeShowCommand SubscriptionsResponse
unsubscribe UnsubscribeShowCommand{..} = withUser doUnsubscribe
    where doUnsubscribe userId = do
            _ <- tvQuery $ unsubscribeShow unsubscribeShowId userId
            subscriptions

downloadShow :: ShowId -> TvApiCall (TvResponse ())
downloadShow showId = tmdbMaybe (Tmdb.getShow showId) doInsertShow
    where
        insertEp ep = tvQuery $ saveEpisode ep
        doInsertSeason season = mapM_ insertEp (tvEpisodesFromTmdbSeason showId season)
        doInsertShow tmdbShow = do
            tvQuery $ saveShow (tvShowFromTmdbShow tmdbShow)
            tmdbSeasons <- tmdb (Tmdb.getSeasons tmdbShow)
            mapM_ doInsertSeason tmdbSeasons
            _ <- tvQuery $ notifyEpisodesAreChanged
            return $ TvSuccess ()

subscribe :: TvCommandHandler SubscribeShowCommand ()
subscribe SubscribeShowCommand{..} = withUser doSubscribe
    where
        subUser userId = do
            _ <- tvQuery $ subscribeShow subscribeShowId userId
            isAiringInFuture <- tvQuery $ isShowAiringInFuture subscribeShowId
            if isAiringInFuture
            then (tvQuery $ setProgressionForUser userId (TvEpisodeId 1 1 subscribeShowId)) >> return ()
            else return ()
        doSubscribe user = do
            inDatabase <- tvQuery $ isShowInDatabase subscribeShowId
            _ <- if (not inDatabase)
                 then do (downloadShow subscribeShowId) >> (subUser user)
                 else subUser user
            return $ TvSuccess ()

toLastEpisodePointer :: TvEpisodePointer -> LastEpisodePointer
toLastEpisodePointer TvEpisodePointer{..} = LastEpisodePointer episodePointerEpisode episodePointerShow

toNextEpisodePointer :: TvEpisodePointer -> NextEpisodePointer
toNextEpisodePointer TvEpisodePointer{..} = NextEpisodePointer episodePointerEpisode episodePointerShow