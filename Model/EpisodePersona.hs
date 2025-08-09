{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- Este módulo é utilizando tanto por Episode.hs quanto por EpisodeSetup.hs
module Model.EpisodePersona where

import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics (Generic)

import Data.Aeson (ToJSON, FromJSON)

-- Representa o personagem que está falando, o E prefixo é Episode
newtype EPeLabel = EPeLabel Text
    deriving (Show, Eq, Ord, Generic)

data EPeNumber
    = EPeNum1
    | EPeNum2
    deriving (Show, Eq, Ord, Generic, ToJSON)

ePeNumberToInt :: EPeNumber -> Int
ePeNumberToInt EPeNum1 = 1
ePeNumberToInt EPeNum2 = 2