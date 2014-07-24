{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Teevy.Database.Query (queryMany, queryMaybe, executeQuery, executeManyQuery, runTvQuery, TvQuery) where

import Control.Monad.Reader
import Database.PostgreSQL.Simple
import qualified Data.List as L
import GHC.Int

newtype TvQuery r = TvQuery { runQuery :: ReaderT Connection IO r } 
                    deriving (Functor, Monad, MonadIO, MonadReader Connection)

runTvQuery :: Connection -> TvQuery r -> IO r
runTvQuery conn q = runReaderT (runQuery q) conn

executeQuery :: ToRow q => Query -> q -> TvQuery Int64
executeQuery qs p = do
	conn <- ask
	liftIO $ execute conn qs p

executeManyQuery :: ToRow q => Query -> [q] -> TvQuery Int64
executeManyQuery qs p = do
    conn <- ask
    liftIO $ executeMany conn qs p

queryMany :: (ToRow q, FromRow r) => Query -> q -> TvQuery [r]
queryMany qs p = do
	conn <- ask
	res <- liftIO $ query conn qs p
	return res

queryMaybe :: (ToRow q, FromRow r) => Query -> q -> TvQuery (Maybe r)
queryMaybe qs p = do
	res <- queryMany qs p
	if containsOne res
	then return . Just . L.head $ res
	else return Nothing

--- Util

containsOne :: [a] -> Bool
containsOne [] = False
containsOne (_x:[]) = True
containsOne (_x:_xs) = False
