{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Teevy.Services.UserService where

import Data.Monoid
import Data.Default


import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (asks)

import Web.JWT

import qualified Data.Aeson as A
import qualified Data.Map as Map
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Crypto.Hash as CHash
import qualified Crypto.BCrypt as BCrypt
import qualified Data.Serialize as Cereal
import qualified Data.Attoparsec.Char8 as P8

import Control.Applicative
import Data.Attoparsec hiding (take)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Clock (getCurrentTime, addUTCTime, NominalDiffTime)
import Crypto.Classes (constTimeEq)
import Crypto.Hash.CryptoAPI (SHA256)
import Crypto.HMAC (hmac', MacKey(..))
import Network.HTTP.Conduit

import Teevy.Text

import Teevy.Database
import Teevy.Database.User
import Teevy.Messages
import Teevy.Services.ApiCall
import Teevy.Services.Validations

import Teevy.Shared.Log
import Teevy.Shared.Config (teevySecret, facebookSecret)

import Prelude hiding (exp)

getUserInfo :: TvQueryHandler UserInfoResponse
getUserInfo = withUser doGetUserInfo
    where
        doGetUserInfo uid = do
            foundUser <- tvQuery $ findUserByUserId uid
            case foundUser of
                Just user -> return $ TvSuccess (UserInfoResponse (userNickname user) "/images/default_user.png")
                Nothing -> return $ TvSystemError "User not found"

ensurePasswordValidatesWithPolicy :: UserId -> TL.Text -> TvApiCall ()
ensurePasswordValidatesWithPolicy userId userPassword = do
    if isHashedWithPreferredPolicy userPassword
    then return ()
    else do
        rehashed <- hashPassword userPassword
        case rehashed of
            Nothing -> return ()
            Just hash -> tvQuery $ updatePassword userId hash >>= \_ -> return()

logAuthError :: TvApiCall ()
logAuthError = do
    remoteAddr <- asks (remoteAddr)
    case remoteAddr of
        Nothing -> return ()
        Just ip -> liftIO $ authErr ip

doLogin :: UserId -> TvApiCall (TvResponse LoginResponse)
doLogin userId = do
    tvSecret <- asks (teevySecret . teevyConfig)
    currentTime <- liftIO $ getCurrentTime
    let sixMonths = 6 * 30 * 24 * 60 * 60 :: NominalDiffTime
    let expires = utcTimeToPOSIXSeconds $ addUTCTime sixMonths currentTime
    let cs = def {
        iss = stringOrURI "teevy.co",
        exp = intDate expires,
        unregisteredClaims = Map.fromList [("userId", (A.toJSON userId))]
    } :: JWTClaimsSet
    let secretKey = secret tvSecret
    let token = encodeSigned HS256 secretKey cs
    return $ TvSuccess $ LoginResponse token

getFacebookMe :: String -> IO (Maybe FacebookMeResponse)
getFacebookMe access_token = do
    let url = "https://graph.facebook.com/v2.0/me?fields=id,name,email&access_token=" ++ access_token
    result <- simpleHttp url
    return $ A.decode result

loginFacebook :: TvCommandHandler FacebookLoginCommand LoginResponse
loginFacebook FacebookLoginCommand{..} = do
    fbSecret <- asks(facebookSecret . teevyConfig)
    let signedRequest = (lazyTextToStrictByteString facebookLoginSignedRequest) <> B8.singleton '\n'
    let parseResult = parse signedRequestParser signedRequest
    case parseResult of
        Done _ (sign, payload) -> do
            case decodeBase64Url payload of
                Left _ -> return $ TvSystemError "Unable to decode signed request"
                Right payLoadJson -> do
                    let decodedFbsr = A.decode $ BL.fromStrict payLoadJson :: Maybe FacebookSignedRequestPayload
                    case decodedFbsr of
                        Nothing -> return $ TvSystemError "Unable to decode signed request"
                        Just fbsr -> do
                            if (fbsrAlgorithm fbsr) == "HMAC-SHA256" then
                                if (isValidRequest fbSecret (sign, payload))
                                then authenticateFacebookUser fbsr
                                else logAuthError >> (return $ TvSystemError "Invalid signed request")
                            else return $ TvSystemError "Unsupported hash algorithm"
        _ -> logAuthError >> (return $ TvSystemError "Invalid signed request")
    where

        authenticateFacebookUser :: FacebookSignedRequestPayload -> TvApiCall (TvResponse LoginResponse)
        authenticateFacebookUser pp = do
            foundFbUser <- findUser
            processedUser <- processUser foundFbUser
            case processedUser of
                Left processError -> return $ TvSystemError (TL.pack processError)
                Right uid -> doLogin uid
            where

                getEmail :: TvApiCall (Maybe Email)
                getEmail = do
                    let at = TL.unpack $ facebookLoginAccessToken
                    fbMe <- liftIO $ getFacebookMe at
                    case fbMe of
                        Nothing -> return Nothing
                        Just(me) -> return $ fbmeEmail me

                processUser :: Maybe TvUser -> TvApiCall (Either String UserId)
                processUser (Just usr) = return $ Right (userId usr)
                processUser (Nothing) = do
                    foundEmail <- getEmail
                    case foundEmail of
                        Nothing -> return $ Left "Unable to fetch email"
                        Just(email) -> do
                            let fbUserId = fbsrUserId pp
                            let nickname = (TL.pack "teevy") <> fbUserId
                            _ <- tvQuery $ updateOrInsertFacebookUser fbUserId nickname email
                            usr <- findUser
                            processUser usr

                findUser :: TvApiCall (Maybe TvUser)
                findUser = do
                    let fbUserId = fbsrUserId pp
                    foundFbUser <- tvQuery $ findUserByFacebookId fbUserId
                    return foundFbUser

        signedRequestParser :: Parser (B.ByteString, B.ByteString)
        signedRequestParser = (,) <$> takeTill isDot <* P8.char8 '.' <*> takeTill P8.isEndOfLine <* P8.endOfLine
            where isDot = inClass "."

        isValidRequest :: B.ByteString -> (B.ByteString, B.ByteString) -> Bool
        isValidRequest fbSecret (sign, payload) =
            case (fmap verify $ decodeBase64Url sign) of
                Left _ -> False
                Right False -> False
                Right True -> True
            where
                hmacKey :: MacKey ctx SHA256
                hmacKey = MacKey fbSecret
                verify decodedSignature = decodedSignature `constTimeEq` (Cereal.encode $ hmac' hmacKey payload)

        addBase64Padding :: B.ByteString -> B.ByteString
        addBase64Padding bs
          | drem == 2 = bs `B.append` "=="
          | drem == 3 = bs `B.append` "="
          | otherwise = bs
          where drem = B.length bs `mod` 4

        decodeBase64Url :: B.ByteString -> Either String B.ByteString
        decodeBase64Url = Base64URL.decode . addBase64Padding

loginTeevy :: TvCommandHandler UserLoginCommand LoginResponse
loginTeevy UserLoginCommand{..} = do
    foundUser <- tvQuery $ findByEmailOrUsername login
    case foundUser of
        Nothing -> logAuthError >> (return $ TvSystemError "Invalid login")
        Just user -> handleFoundUser user
    where
        handleFoundUser TvUser { userId = uid, userPassword = Just(pwd), oldPassword = Nothing, oldSalt = Nothing } = do
            valid <- isPasswordValid loginPassword pwd
            if valid then do
                ensurePasswordValidatesWithPolicy uid pwd
                doLogin uid
            else logAuthError >> (return $ TvSystemError "Invalid login")

        handleFoundUser TvUser { userId = uid, userPassword = Nothing, oldPassword = Just(pwd), oldSalt = Just(slt) } = do
            let password = lazyTextToStrictByteString loginPassword
            let salt = lazyTextToStrictByteString slt
            let storedPassword = lazyTextToStrictByteString pwd
            let crypted = encrypt 20 (password <> salt)
            if crypted == storedPassword then do
                newPassword <- hashPassword loginPassword
                case newPassword of
                    Nothing -> return $ TvSystemError "Unable to login due an internal error"
                    Just p -> (tvQuery $ updatePassword uid p) >> (doLogin uid)
            else logAuthError >> (return $ TvSystemError "Invalid login")

        handleFoundUser _ = return $ TvSystemError "Unable to login user"


registerUser :: TvCommandHandler UserRegisterCommand LoginResponse
registerUser cmd@UserRegisterCommand{..} = do
    
    let errors = runTvValidations cmd
    
    if null errors then do
        emailExists <- tvQuery $ userWithEmailExists registerEmail
        nicknameExists <- tvQuery $ userWithNicknameExists registerNickname
        if emailExists || nicknameExists then do
            return $ TvInputErrors [TvInputError "registerEmail" "User with email/nickname already exists"]
        else do
            cryptedPassword <- hashPassword registerPassword
            case cryptedPassword of
                Nothing -> return $ TvSystemError "Unable to register due an internal error"
                Just password -> do
                    _ <- tvQuery $ insertUser registerEmail password registerNickname
                    loginTeevy (UserLoginCommand registerEmail registerPassword)
    else do
        return $ TvInputErrors errors


-----------------
-- OLD ENCRYPTION
-----------------

sha512 :: B.ByteString -> CHash.Digest CHash.SHA512
sha512 = CHash.hash

iterateN :: Int -> (a -> a) -> a -> a
iterateN iterations iterator seed = last $ take iterations src
    where 
        src = iterate iterator (iterator seed)

encrypt :: Int -> B.ByteString -> B.ByteString
encrypt iterations seed = iterateN iterations iterator seed
    where
        iterator = CHash.digestToHexByteString . sha512

--------------------
-- BCRYPT ENCRYPTION
--------------------

policy :: BCrypt.HashingPolicy
policy = BCrypt.slowerBcryptHashingPolicy

hashPassword :: TL.Text -> TvApiCall (Maybe TL.Text)
hashPassword pwd = do
    hash <- liftIO $ BCrypt.hashPasswordUsingPolicy policy (lazyTextToStrictByteString pwd)
    return $ fmap strictByteStringToLazyText hash

isHashedWithPreferredPolicy :: TL.Text -> Bool
isHashedWithPreferredPolicy pwd = BCrypt.hashUsesPolicy policy (lazyTextToStrictByteString pwd)

isPasswordValid :: TL.Text -> TL.Text -> TvApiCall Bool
isPasswordValid plainPwd hashPwd = do
    return $ BCrypt.validatePassword
                (lazyTextToStrictByteString hashPwd)
                (lazyTextToStrictByteString plainPwd)