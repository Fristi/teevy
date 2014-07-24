module Trakt(
    module Trakt.Calls,
    module Trakt.Models,
    Client.runTraktCall, Client.TraktCall
) where

import Trakt.Models
import Trakt.Calls
import Trakt.Instances()

import qualified Trakt.Client as Client