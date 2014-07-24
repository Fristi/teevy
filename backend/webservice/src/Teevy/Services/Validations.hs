{-# LANGUAGE RecordWildCards, OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Teevy.Services.Validations  where

import qualified Data.Text.Lazy as TL
import Data.Int
import qualified Text.Email.Validate as EV

import Control.Monad.Writer

import Teevy.Text
import Teevy.Messages


newtype TvValidation a = TvValidation { runValidation :: Writer [TvInputError] a }
                         deriving (Monad, MonadWriter [TvInputError])

class TvValidator a where
    validate :: a -> TvValidation ()

runTvValidations :: (TvValidator a) => a -> [TvInputError]
runTvValidations s = snd $ runWriter (runValidation (validate s))

ensure :: Bool -> TvInputError -> TvValidation ()
ensure b msg = unless b $ tell [msg]

validEmail :: TL.Text -> Bool
validEmail = EV.isValid . lazyTextToStrictByteString

lengthBetween :: Int64 -> Int64 -> TL.Text -> Bool
lengthBetween low up str = len >= low && len <= up
	where
		len = TL.length str

instance TvValidator UserRegisterCommand where
	validate UserRegisterCommand{..} = do
		
		ensure (validEmail registerEmail)
			$ TvInputError "registerEmail" "Invalid email"

		ensure (lengthBetween 2 32 registerNickname) 
			$ TvInputError "registerNickname" "Username should be between 2 and 32 characters"

		ensure (lengthBetween 8 32 registerPassword) 
			$ TvInputError "registerPassword" "Password should be between 8 and 32 characters"