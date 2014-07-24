module Teevy.Services.LoginFacebook where


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

authenticateFacebookUser :: FacebookSignedRequestPayload -> TvApiCall (TvResponse LoginResponse)
authenticateFacebookUser pp = do
    foundFbUser <- findUser
    processedUser <- processUser foundFbUser
    case processedUser of
        Left processError -> return $ TvSystemError (TL.pack processError)
        Right uid -> doLogin uid


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