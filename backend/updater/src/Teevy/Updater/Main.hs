import System.Environment
import System.Exit

import Control.Exception
import Control.Monad (when)

import Teevy.Shared.Log
import Teevy.Shared.Config
import Teevy.Shared.TmdbConversion

import Teevy.Database.Show
import Teevy.Database.Subscription (markWatchedAllFalse)
import Teevy.Database

import Database.PostgreSQL.Simple
import Network (withSocketsDo)

import qualified Tmdb as Tmdb
import qualified Trakt as Trakt
import qualified Data.Foldable as F
import qualified Data.Text.Lazy as TL


updateAirDatesForSeason :: Connection -> Int -> String -> Tmdb.TmdbShowSeason -> IO ()
updateAirDatesForSeason conn sid sname season = do
    let seasonNr = Tmdb.showSeasonNumber season
    res <- Trakt.runTraktCall (Trakt.getSeason sname seasonNr) "9c850801a91ac97fe69f584ad5b769e7"
    case res of
        Just episodes -> do
            info "updater" ("-> data returned from trakt for showId: " ++ (show sid) ++ " season: " ++ (show seasonNr))
            mapM_ (updateEpisode seasonNr) episodes
        Nothing -> info "updater" ("no data return from trakt for showId: " ++ (show sid) ++ " season: " ++ (show seasonNr))

    where
        updateEpisode seasonNr ep = do
            let episodeNr = Trakt.traktEpisodeNumber ep
            let airDate = Trakt.traktEpisodeFirstAired ep
            runTvQuery conn (updateEpisodeInfo sid seasonNr episodeNr airDate)

getAndStoreEpisodes :: Connection -> String -> Tmdb.TmdbTvShow -> IO()
getAndStoreEpisodes conn key shw = do
    foundSeasons <- Tmdb.runTmdbCall (Tmdb.getSeasons shw) key
    let tvEpisodes = F.foldMap (tvEpisodesFromTmdbSeason sid) foundSeasons
    info "updater" ("found " ++ show (length foundSeasons) ++ " seasons (for: " ++ show sid ++ ")")
    info "updater" ("found " ++ show (length tvEpisodes) ++ " episodes (for: " ++ show sid ++ ")")

    mapM_ insertEp tvEpisodes
    mapM_ (updateAirDatesForSeason conn sid sname) foundSeasons
    where
        insertEp ep = runTvQuery conn (saveEpisode ep)
        sid = Tmdb.showId shw
        sname = TL.unpack $ Tmdb.showName shw

getAndStoreShow :: Connection -> String -> Int -> IO()
getAndStoreShow conn key sid = do
    info "updater" ("downloading show: " ++ show sid)
    foundShow <- Tmdb.runTmdbCall (Tmdb.getShow sid) key
    case foundShow of
        Nothing -> do
            info "updater" ("show with id " ++ show sid ++ " not found")
            return ()
        Just shw -> do
            let updatedTvShow = tvShowFromTmdbShow shw
            foundCurrentTvShow <- runTvQuery conn (getShow sid)
            runTvQuery conn (saveShow updatedTvShow)
            let updatedInProduction = showInProduction updatedTvShow
            info "updater" ("show with id " ++ show sid ++ " in production: " ++ (show updatedInProduction))
            when (updatedInProduction) $ do
                getAndStoreEpisodes conn key shw
            case foundCurrentTvShow of
                Just currentTvShow -> do
                    let currentInProduction = showInProduction currentTvShow
                    if currentInProduction == False && updatedInProduction == True
                    then do
                        info "updater" ("==> detected that show has gone from inProduction: false -> true, changing subscriptions")
                        _ <- runTvQuery conn (markWatchedAllFalse sid)
                        return ()
                    else return ()
                Nothing -> return ()




updateDatabase :: TL.Text -> Connection -> IO ()
updateDatabase key conn = do
    showIds <- runTvQuery conn getShowIds
    info "updater" ("found " ++ show showIds ++ " showIds")
    mapM_ (getAndStoreShow conn (TL.unpack key)) showIds
    _ <- runTvQuery conn notifyEpisodesAreChanged
    return ()
    
parse :: [String] -> IO()
parse [] = putStrLn "usage: teevy-updater <config-path>" >> exitWith (ExitFailure 1)
parse (f:_) = do
    cfg <- loadConfig f
    case cfg of
        Just c -> bracket (connect (teevyConnectInfo c)) close (updateDatabase (tmdbApiKey c))
        Nothing -> err "main" "Error loading config..."
        
main :: IO()
main = withSocketsDo $ getArgs >>= parse