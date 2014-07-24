module Teevy.Shared.TmdbConversion where

import Teevy.Database.Models
import Data.Maybe (isJust, fromJust)

import qualified Tmdb as Tmdb

tvShowFromTmdbShow :: Tmdb.TmdbTvShow -> TvShow
tvShowFromTmdbShow tmdbShow = TvShow
    (Tmdb.showId tmdbShow)
    (Tmdb.showName tmdbShow)
    (Tmdb.showPosterPath tmdbShow)
    (Tmdb.showInProduction tmdbShow)
    (Tmdb.showFirstAirDate tmdbShow) 
    (Tmdb.showLastAirDate tmdbShow) 

tvEpisodesFromTmdbSeason :: ShowId -> Tmdb.TmdbShowSeason -> [TvEpisode]
tvEpisodesFromTmdbSeason sid tmdbSeason = map toTvEpsisode filteredEps
    where
        toTvEpsisode ep = TvEpisode
            (epId $ fromJust(Tmdb.episodeNr ep))
            (Tmdb.episodeTitle ep) 
            (Tmdb.episodeAirdate ep)

        eps = Tmdb.showSeasonEpisodes tmdbSeason
        epId episodeNr = TvEpisodeId seasonNr episodeNr sid
        filteredEps = filter (isJust . Tmdb.episodeNr) eps
        seasonNr = Tmdb.showSeasonNumber tmdbSeason