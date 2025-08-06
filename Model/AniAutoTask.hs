{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- MyModule.hs
module Model.AniAutoTask where

import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics (Generic) 

data AniAutoTask = AniAutoTask
    { atTaskName :: Text -- temp
    , atDescription :: Text -- temp
    } deriving (Show, Eq, Generic)