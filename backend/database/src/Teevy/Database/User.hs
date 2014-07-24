{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Teevy.Database.User where

import Data.Maybe (isJust)
import Control.Monad (when)

import Database.PostgreSQL.Simple

import Teevy.Database.Models
import Teevy.Database.Query
import Teevy.Database.Instances()

import qualified Data.Text.Lazy as TL
import GHC.Int

loweredAndStripped :: TL.Text -> TL.Text
loweredAndStripped = TL.toLower . TL.strip

-- Facebook users

findUserByFacebookId :: FacebookUserId -> TvQuery (Maybe TvUser)
findUserByFacebookId fbid = queryMaybe "select * from users where facebook_user_id = ?" (Only fbid)

userWithFacebookIdExists :: FacebookUserId -> TvQuery Bool
userWithFacebookIdExists fbid = findUserByFacebookId fbid >>= return . isJust

insertFacebookUser :: FacebookUserId -> Nickname -> Email -> TvQuery Int64
insertFacebookUser fbid nick email = executeQuery "insert into users (facebook_user_id, nickname, email) values (?,?,?)" (fbid, nick, email)

updateUserToFacebook :: FacebookUserId -> Email -> TvQuery Int64
updateUserToFacebook fbid email = executeQuery "update users set facebook_user_id = ? where email = ?" (fbid, loweredAndStripped email)

updateOrInsertFacebookUser :: FacebookUserId -> Nickname -> Email -> TvQuery ()
updateOrInsertFacebookUser fbid nick email = do
    affected <- updateUserToFacebook fbid email
    when (affected == 0) $ do
        _ <- insertFacebookUser fbid nick email
        return ()

-- All users

numUsers :: TvQuery (Maybe Int)
numUsers = queryMaybe "select count(0) from users" ()

findUserByUserId :: UserId -> TvQuery (Maybe TvUser)
findUserByUserId uid = queryMaybe "select * from users where id = ?" (Only uid)

findUserByNickname :: Nickname -> TvQuery (Maybe TvUser)
findUserByNickname nick = queryMaybe "select * from users where lower(nickname) = ?" (Only $ loweredAndStripped nick)

userWithNicknameExists :: Nickname -> TvQuery Bool
userWithNicknameExists nick = findUserByNickname nick >>= return . isJust

findByEmailOrUsername :: TL.Text -> TvQuery (Maybe TvUser)
findByEmailOrUsername login = queryMaybe "select * from users where lower(email) = ? or lower(nickname) = ?" (loweredLogin, loweredLogin)
    where
        loweredLogin = loweredAndStripped login

updatePassword :: UserId -> Password -> TvQuery Int64
updatePassword uid pwd = executeQuery "update users set password = ?, old_password = null, old_salt = null where id = ?" (pwd, uid)

insertUser :: Email -> Password -> Nickname -> TvQuery Int64
insertUser email pwd nick = executeQuery "insert into users (email, nickname, password) values (?,?,?)" (loweredAndStripped email, nick, pwd)

findUserByEmail :: Email -> TvQuery (Maybe TvUser)
findUserByEmail email = queryMaybe "select * from users where lower(email) = ?" (Only $ loweredAndStripped email)

userWithEmailExists :: Email -> TvQuery Bool
userWithEmailExists email = findUserByEmail email >>= return . isJust
