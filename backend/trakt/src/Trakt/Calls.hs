module Trakt.Calls where

import Trakt.Models
import Trakt.Client
import Trakt.Instances()
import Text.Printf
import Data.Char

getSeason :: String -> Int -> TraktCall (Maybe [TraktEpisode])
getSeason name seasonNr = fetch url
    where
        url = printf "http://api.trakt.tv/show/season.json/__apikey__/%s/%s" (showName name) (show seasonNr)
        showName =  map toLower . map spaceToDash . filter onlyNumberAndLetter . replace "&" "and"
        onlyNumberAndLetter c = isLetter c || isNumber c || isSpace c
        spaceToDash c = if isSpace c then '-' else c