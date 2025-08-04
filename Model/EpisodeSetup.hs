{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- MyModule.hs
module Model.EpisodeSetup where

import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics (Generic)

import Model.EpisodeChar (ECharLabel(..))

-- Informações adicionais do episódio
data EpisodeSetup = EpisodeSetup
    { sSprites :: [SSprite]
    , sBackgroundImage :: SBackground
    } deriving (Show, Eq, Generic)

data SSprite = SSprite
    { sLabel :: ECharLabel   -- Nome do sprite
    , sPsdPath :: FilePath     -- Caminho do sprite
    , sNumber :: SSpriteNumber -- Número do sprite (posição no background)
    } deriving (Show, Eq, Generic)

-- para identificar o sprite nas falas e no background
-- para saber qual posição o sprite ocupa no background
data SSpriteNumber 
    = SNumber1
    | SNumber2
    deriving (Show, Eq, Generic)

data SBackground = SBackground
    { bImagePath :: FilePath -- Caminho da imagem de fundo
    , bWidth :: Int -- Largura da imagem
    , bHeight :: Int -- Altura da imagem
    , bSpritePositions :: BSpritePositions -- Posições dos sprites no fundo
    } deriving (Show, Eq, Generic)

data BSpritePositions
    = SPositionsFor1
        {pFor1Sprite :: PSprite }
    | SPositionsFor2
        { pFor2Sprite1 :: PSprite
        , pFor2Sprite2 :: PSprite }
    deriving (Show, Eq, Generic)

data PSprite = PSprite
    { sX :: Int -- Posição X do sprite no fundo
    , sY :: Int -- Posição Y do sprite no fundo
    } deriving (Show, Eq, Generic)

exampleEpisodeSetup :: EpisodeSetup
exampleEpisodeSetup = EpisodeSetup
    { sSprites = 
        [ SSprite (ECharLabel "char_felipe") "path/to/sprite1.psd" SNumber1
        , SSprite (ECharLabel "char_gisele") "path/to/sprite2.psd" SNumber2
        ]
    , sBackgroundImage = SBackground
        { bImagePath = "path/to/background.jpg"
        , bWidth = 800
        , bHeight = 600
        , bSpritePositions = SPositionsFor2
            { pFor2Sprite1 = PSprite 100 150
            , pFor2Sprite2 = PSprite 200 250
            }
        }
    }