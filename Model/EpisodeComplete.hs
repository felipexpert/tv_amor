{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- MyModule.hs
module Model.EpisodeComplete where

import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics (Generic) 

import Model.Episode
import Model.EpisodeSetup
import Model.AniAutoTask


data EpisodeComplete = EpisodeComplete 
    { ecEpisode :: Episode
    , ecEpisodeSetup :: EpisodeSetup
    } deriving (Show, Eq, Generic)

episodeCompleteToAniAutoTaskIO :: EpisodeComplete -> IO AniAutoTask
episodeCompleteToAniAutoTaskIO ec = undefined