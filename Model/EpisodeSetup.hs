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
    { sprites :: [Sprite]
    , backgroundImage :: FilePath -- Caminho para a imagem de fundo
    } deriving (Show, Eq, Generic)

data ESSprite = ESSprite
    { essLabel :: ECharLabel   -- Nome do sprite
    , essPSDPath :: FilePath     -- Caminho do sprite
    , essNumber :: ESSpriteNumber -- Número do sprite (posição no background)
    } deriving (Show, Eq, Generic)

-- para identificar o sprite nas falas e no background
-- para saber qual posição o sprite ocupa no background
data ESSpriteNumber 
    = essn1
    | essn2
    deriving (Show, Eq, Generic)

data ESBackground = ESBackground
    { esbImagePath :: FilePath -- Caminho da imagem de fundo
    , esbWidth :: Int -- Largura da imagem
    , esbHeight :: Int -- Altura da imagem
    , esbSpritePositions :: ESBackgroundSpritePositions -- Posições dos sprites no fundo
    } deriving (Show, Eq, Generic)

data ESBackgroundSpritePositions
    = ESBackgroundSpritePositions1
        { esbsp1Sprite :: ESBackgroundSpritePositionsSprite } deriving (Show, Eq, Generic)
    | ESBackgroundSpritePositions2
        { esbsp2Sprite1 :: ESBackgroundSpritePositionsSprite
        , esbsp2Sprite2 :: ESBackgroundSpritePositionsSprite } deriving (Show, Eq, Generic)

data ESBackgroundSpritePositionsSprite = ESBackgroundSpritePositionsSprite
    { esbspsX :: Int -- Posição X do sprite no fundo
    , esbspsY :: Int -- Posição Y do sprite no fundo
    } deriving (Show, Eq, Generic)