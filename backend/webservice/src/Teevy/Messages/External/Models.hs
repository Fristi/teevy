{-# LANGUAGE DeriveGeneric #-}
module Teevy.Messages.External.Models where

import qualified Data.Text.Lazy as TL


data FacebookLoginCommand = FacebookLoginCommand {
    facebookLoginAccessToken :: !TL.Text,
    facebookLoginExpiresIn :: Int,
    facebookLoginSignedRequest :: !TL.Text,
    facebookLoginUserId :: !TL.Text
} deriving (Show)

data FacebookSignedRequestPayload = FacebookSignedRequestPayload {
    fbsrUserId :: TL.Text,
    fbsrAlgorithm :: TL.Text,
    fbsrCode :: TL.Text
}

data FacebookMeResponse = FacebookMeResponse {
    fbmeUserId :: Maybe TL.Text,
    fbmeEmail :: Maybe TL.Text
}