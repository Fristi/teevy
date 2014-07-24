{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import System.Environment
import System.Exit

import Control.Monad (forever)
import Control.Concurrent (forkIO)

import Web.Scotty

import Teevy.Scotty

import Teevy.Database
import Teevy.Database.Show (updateEpisodesTable)

import Teevy.Shared.Log
import Teevy.Shared.Config

import Data.Pool
import qualified Database.PostgreSQL.Simple as P
import qualified Database.PostgreSQL.Simple.Notification as PN

import qualified Teevy.Services.StatService as StatService
import qualified Teevy.Services.UserService as UserService
import qualified Teevy.Services.ShowService as ShowService
import qualified Teevy.Services.SubscriptionService as SubscriptionService
import qualified Teevy.Services.ExploreService as ExploreService

webService :: TvConfig -> Pool P.Connection -> IO ()
webService cfg pool = scotty (teevyPort cfg) $ do
    
    defaultHandler exceptionHandler

    -- User service
    get "/user/info" $ doQuery cfg pool UserService.getUserInfo
    post "/user/register" $ doCommand cfg pool UserService.registerUser

    post "/user/auth/teevy" $ doCommand cfg pool UserService.loginTeevy
    post "/user/auth/facebook" $ doCommand cfg pool UserService.loginFacebook

    -- Shows service
    get "/show/:showId/episodes" $ do
        sid <- param "showId"
        doQuery cfg pool (ShowService.getEpisodes sid)

    -- Explore
    get "/explore" $ doQuery cfg pool ExploreService.explore
    post "/search" $ doCommand cfg pool ExploreService.search

    -- Subscription service
    get "/subscriptions" $ doQuery cfg pool SubscriptionService.subscriptions
    post "/subscription/add" $ doCommand cfg pool SubscriptionService.subscribe
    post "/subscription/remove" $ doCommand cfg pool SubscriptionService.unsubscribe
    post "/subscription/progression" $ doCommand cfg pool SubscriptionService.setProgression
    post "/subscription/progression/next" $ doCommand cfg pool SubscriptionService.nextEpisode

    -- Stats
    get "/stats" $ doQuery cfg pool StatService.getWebsiteStats

    notFound notFoundHandler
    
connectToDb :: TvConfig -> IO P.Connection
connectToDb cfg = P.connect (teevyConnectInfo cfg)

connectionPool :: TvConfig -> IO (Pool P.Connection)
connectionPool cfg = createPool (connectToDb cfg) P.close 5 5 20

update :: P.Connection -> PN.Notification -> IO ()
update conn _ = (runTvQuery conn updateEpisodesTable) >> (info "dbNotification" "episodesMutated got called")

episodesMutatedListener :: P.Connection -> IO ()
episodesMutatedListener c = listen >> loop
    where
        listen = P.execute_ c "LISTEN episodesMutated"
        loop = forever $ PN.getNotification c >>= (update c)

parse :: [String] -> IO()
parse [] = putStrLn "usage: teevy-backend <config-path>" >> exitWith (ExitFailure 1)
parse (f:_) = do
    loadedConfig <- loadConfig f
    case loadedConfig of
        Just cfg -> do
            pool <- connectionPool cfg
            listnerConn <- connectToDb cfg
            _ <- forkIO $ episodesMutatedListener listnerConn
            webService cfg pool
        Nothing -> err "main" "Error loading config..."
        
main :: IO()
main = getArgs >>= parse