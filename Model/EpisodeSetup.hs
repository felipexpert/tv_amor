{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

-- MyModule.hs
module Model.EpisodeSetup where

import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics (Generic)

import Model.EpisodePersona (EPeLabel(..), EPeNumber(..))

import System.FilePath (replaceExtension, (</>))

import qualified Data.List as List

import Data.Maybe (catMaybes)

import qualified Model.Config as C
import Model.AudiosInfo (AudioRequestConfig(..))

import Data.Aeson (decode)

import qualified Data.ByteString.Lazy as B

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
        [ SSprite (EPeLabel "pe_felipe777") "02 sprite mega-sushi-temakeria IMPORT.psd" EPeNum1
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

loadAllAudioRequestConfigsIO :: EpisodeSetup -> IO [(AudioRequestConfig, EPeLabel)]
loadAllAudioRequestConfigsIO es = do
  configOpts <- mapM  mapperIO peLabels
  let configs = catMaybes configOpts
  return configs
  where
    peLabels :: [EPeLabel]
    peLabels = (fmap sLabel) . sSprites $ es
    mapperIO :: EPeLabel -> IO (Maybe (AudioRequestConfig, EPeLabel))
    mapperIO peLabel = do
      configOpt <- loadAudioRequestConfigOptIO es peLabel
      let configOpt' = fmap (,peLabel) configOpt
      return configOpt'


-- Lê o arquivo "config.json" na raiz do projeto e retorna o Config
loadAudioRequestConfigOptIO :: EpisodeSetup -> EPeLabel -> IO (Maybe AudioRequestConfig)
loadAudioRequestConfigOptIO es peLabel = do
  case configFileOpt of
    Just configFile -> loadAudioRequestConfigOptIO' configFile
    _ -> return Nothing
  where
    configFileOpt = episodeSetupAudioRequestConfigFile es peLabel
    loadAudioRequestConfigOptIO' :: FilePath -> IO (Maybe AudioRequestConfig)
    loadAudioRequestConfigOptIO' configPath = do

      conf <- C.loadConfigIO
      let workingDir = C.workingDir conf

      let path = workingDir </> configPath
      content <- B.readFile path
      case decode content of
        Just cfg -> return (Just cfg)
        Nothing -> return Nothing

episodeSetupAudioRequestConfigFiles :: EpisodeSetup -> [(FilePath, EPeLabel)]
episodeSetupAudioRequestConfigFiles es = fmap mapper sprites
  where
    sprites = sSprites es
    mapper s = (psdToJson (sPsdPath s), sLabel s)

episodeSetupAudioRequestConfigFile :: EpisodeSetup -> EPeLabel ->  Maybe FilePath
episodeSetupAudioRequestConfigFile es peLabel = configFileOpt
  where
    sprites = sSprites es
    thisSpriteOpt = List.find ((peLabel ==) . sLabel) sprites
    spriteFPOpt :: Maybe FilePath
    spriteFPOpt = fmap sPsdPath thisSpriteOpt
    configFileOpt :: Maybe FilePath
    configFileOpt = fmap psdToJson spriteFPOpt

-- converte o tipo psd para json
psdToJson :: FilePath -> FilePath
psdToJson path = replaceExtension path "json"