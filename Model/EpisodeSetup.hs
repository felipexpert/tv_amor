{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- MyModule.hs
module Model.EpisodeSetup where

import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics (Generic)

import Model.EpisodePersona (EPeLabel(..), EPeNumber(..))

-- Informações adicionais do episódio
data EpisodeSetup = EpisodeSetup
    { sSprites :: [SSprite]
    , sBackgroundImage :: SBackground
    } deriving (Show, Eq, Generic)

data SSprite = SSprite
    { sLabel :: EPeLabel   -- Nome do sprite
    , sPsdPath :: FilePath     -- Caminho do sprite
    , sNumber :: EPeNumber -- Número do sprite (posição no background)
    } deriving (Show, Eq, Generic)

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

bSpritePositionsList :: BSpritePositions -> [PSpriteNumbered]
bSpritePositionsList (SPositionsFor1 sprite) =
    [ PSpriteNumbered sprite EPeNum1 ]
bSpritePositionsList (SPositionsFor2 sprite1 sprite2) =
    [ PSpriteNumbered sprite1 EPeNum1
    , PSpriteNumbered sprite2 EPeNum2]

data PSprite = PSprite
    { sX :: Int -- Posição X do sprite no fundo
    , sY :: Int -- Posição Y do sprite no fundo
    } deriving (Show, Eq, Generic)

data PSpriteNumbered = PSpriteNumbered
    { snSprite :: PSprite
    , snNumber :: EPeNumber 
    } deriving (Show, Eq, Generic)


exampleEpisodeSetup :: EpisodeSetup
exampleEpisodeSetup = EpisodeSetup
    { sSprites = 
        [ SSprite (EPeLabel "pe_damiao") "02 sprite mega-sushi-temakeria IMPORT.psd" EPeNum1
        , SSprite (EPeLabel "pe_felipe") "01 sprite melhores-ofertas IMPORT.psd" EPeNum2
        ]
    , sBackgroundImage = SBackground
        { bImagePath = "01_escritorio.png"
        , bWidth = 1536
        , bHeight = 1024
        , bSpritePositions = SPositionsFor2
            { pFor2Sprite1 = PSprite (-100) (-50)
            , pFor2Sprite2 = PSprite 100 (-50)
            }
        }
    }