{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Teevy.Messages.External.Instances where

import Data.Aeson
import Control.Monad (mzero)
import Control.Applicative

import Teevy.Messages.External.Models

instance FromJSON FacebookLoginCommand where
    parseJSON (Object v) = FacebookLoginCommand <$> (v .: "accessToken") <*> (v .: "expiresIn") <*> (v .: "signedRequest") <*> (v .: "userID")
    parseJSON _ = mzero

instance FromJSON FacebookSignedRequestPayload where
    parseJSON (Object v) = FacebookSignedRequestPayload <$> (v .: "user_id") <*> (v .: "algorithm") <*> (v .: "code")
    parseJSON _ = mzero

instance FromJSON FacebookMeResponse where
    parseJSON (Object v) = FacebookMeResponse <$> (v .:? "user_id") <*> (v .:? "email")
    parseJSON _ = mzero

