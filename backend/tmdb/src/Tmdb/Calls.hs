module Tmdb.Calls where

import Tmdb.Models
import Tmdb.Client
import Tmdb.Instances()

getShow :: ShowId -> TmdbCall (Maybe TmdbTvShow)
getShow sid = fetch ("http://api.themoviedb.org/3/tv/" ++ show sid) []

getSeason :: ShowId -> SeasonNr -> TmdbCall (Maybe TmdbShowSeason)
getSeason sid seasonNr = fetch ("http://api.themoviedb.org/3/tv/" ++ show sid ++ "/season/" ++ show seasonNr) []

getSeasons :: TmdbTvShow -> TmdbCall [TmdbShowSeason]
getSeasons shw = fetchConcurrent urls 5
    where
        seasons = showSeasons shw
        sid = showId shw
        
        seasonNumbers = map seasonNumber seasons
        filteredSeasonNumbers = filter (\x -> x > 0) seasonNumbers
        urls = map urlMapper filteredSeasonNumbers
        urlMapper s = "http://api.themoviedb.org/3/tv/" ++ show sid ++ "/season/" ++ show s

search :: SearchTerm -> TmdbCall (Maybe (TmdbResult TmdbSearchResult))
search term = fetch ("http://api.themoviedb.org/3/search/tv") [("query", term), ("search_type", "ngram"), ("sort_order", "desc"),("sort_by", "popularity")]