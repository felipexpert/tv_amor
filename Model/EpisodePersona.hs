{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- Este módulo é utilizando tanto por Episode.hs quanto por EpisodeSetup.hs
module Model.EpisodePersona where

import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics (Generic)



-- Representa o personagem que está falando, o E prefixo é Episode
newtype EPeLabel = EPeLabel Text
    deriving (Show, Eq, Ord, Generic)

data EPeNumber
    = EPeNum1
    | EPeNum2
    deriving (Show, Eq, Ord, Generic)