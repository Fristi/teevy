{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, DeriveGeneric #-}
module Teevy.Services.ApiCall where

import Control.Monad.Reader

import Data.Pool
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)

import qualified Data.Aeson as A
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Map as Map

import Tmdb

import Teevy.Database
import Teevy.Messages

import Teevy.Shared.Config (TvConfig(..))

import Database.PostgreSQL.Simple
import Web.JWT

import Prelude hiding (exp)

-- Types

data TvContext = TvContext {
		teevyConfig :: TvConfig
	,	dbConnection :: Connection
	,	userSessionToken :: Maybe TL.Text
	,	remoteAddr :: Maybe TL.Text
}

type TvCommandHandler i o = (i -> TvApiCall (TvResponse o))
type TvQueryHandler o = TvApiCall (TvResponse o)

newtype TvApiCall r = TvApiCall { runCall :: ReaderT TvContext IO r } 
                      deriving (Functor, Monad, MonadIO, MonadReader TvContext)

-- Api call combinators

runTvApiCall :: TvApiCall r -> TvConfig -> Pool Connection -> Maybe TL.Text -> Maybe TL.Text -> IO r
runTvApiCall call cfg pool sessionToken addr = 
    withResource pool run
    where
        run c = runReaderT (runCall call) (TvContext cfg c sessionToken addr)

getUser :: TvApiCall (Maybe UserId)
getUser = do
    sessionToken <- asks (userSessionToken)
    tvSecret <- asks (teevySecret . teevyConfig)
    currentTime <- liftIO $ getPOSIXTime
    return $ processToken tvSecret sessionToken currentTime
    where
        processParsedJson :: A.Result a -> Maybe a
        processParsedJson (A.Success r) = Just r
        processParsedJson _ = Nothing

        isNotExpired :: POSIXTime -> IntDate -> Bool
        isNotExpired currentTime expiredIn = (secondsSinceEpoch expiredIn) > currentTime

        processToken :: T.Text -> Maybe TL.Text -> POSIXTime -> Maybe Int
        processToken secretKey foundToken currentTime = do
            jwtToken <- foundToken
            let strictJwtToken = T.concat . TL.toChunks $ jwtToken
            jwt <- decodeAndVerifySignature (secret secretKey) strictJwtToken
            let cs = claims jwt
            expires <- exp cs
            guard(isNotExpired currentTime expires)
            let unc = unregisteredClaims cs
            foundUserId <- Map.lookup "userId" unc
            let parsedUserId = A.fromJSON foundUserId
            uid <- processParsedJson parsedUserId
            return uid

withUser :: A.ToJSON o => (UserId -> TvApiCall (TvResponse o)) -> TvApiCall (TvResponse o)
withUser callback = do
    foundUserId <- getUser
    case foundUserId of
        Nothing -> return $ TvSystemError "No user found on session"
        Just uid -> callback uid

tvQuery :: TvQuery r -> TvApiCall r
tvQuery q = do 
    conn <- asks dbConnection
    res <- liftIO $ runTvQuery conn q
    return res

tmdb :: (A.FromJSON r) => TmdbCall r -> TvApiCall r
tmdb call = do
    apiKey <- asks (tmdbApiKey . teevyConfig)
    liftIO $ runTmdbCall call (TL.unpack apiKey)

tmdbMaybe :: (A.ToJSON o, A.FromJSON r) => TmdbCall(Maybe r) -> (r -> TvApiCall (TvResponse o)) -> TvApiCall (TvResponse o)
tmdbMaybe call cont = do
    res <- tmdb call
    case res of
        Nothing -> return $ TvSystemError "Something went wrong while contacting the movie db, please try again.."
        Just result -> cont result