{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-tabs #-}

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

import Data.Aeson (decode, ToJSON, FromJSON)

import qualified Data.ByteString.Lazy as B

data GestureApplicationType
	= GATDefault
	| GATWithoutGestureStayStatic
	| GATWithoutGestureStayNormal
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data CustomExtraPrefs = CustomExtraPrefs
  { cepGestureApplicationType :: GestureApplicationType }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data EpisodeSetupLoader = EpisodeSetupLoader
  { eslSprites :: [SSprite]
  , eslBackgroundImageJSON :: FilePath -- <nome-do-arquivo>.json
  , eslCustomExtraPrefsJSONOpt :: Maybe FilePath -- <nome-do-arquivo>.json
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

loadEpisodeSetupIO :: EpisodeSetupLoader -> IO EpisodeSetup
loadEpisodeSetupIO loader = do
  -- carregar as configuracoes
  config <- C.loadConfigIO
  bg <- loadBGIO config
  customExtraPrefsOpt <- loadCustomExtraPrefsIO config
  return EpisodeSetup
    { sSprites = eslSprites loader
    , sBackgroundImage = bg
    , sCustomExtraPrefsOpt = customExtraPrefsOpt
    }
  where
    loadBGIO :: C.Config -> IO SBackground
    loadBGIO config = do
      let fileJSON = eslBackgroundImageJSON loader
      let pathJSON = bgDir </> fileJSON
      content <- B.readFile pathJSON
      case decode content of
        Just bg -> return bg
        Nothing -> error $ "Erro ao decodificar o arquivo de background: " <> pathJSON
      where
        bgDir = C.backgroundDir config
    loadCustomExtraPrefsIO :: C.Config -> IO (Maybe CustomExtraPrefs)
    loadCustomExtraPrefsIO config = do
      case eslCustomExtraPrefsJSONOpt loader of
        Just fileJSON -> do
          let pathJSON = customDir </> fileJSON
          content <- B.readFile pathJSON
          case decode content of
            Just prefs -> return (Just prefs)
            Nothing -> error $ "Erro ao decodificar o arquivo de custom extra prefs: " <> pathJSON
        Nothing -> return Nothing
      where
        customDir = C.customDir config

-- Informações adicionais do episódio
data EpisodeSetup = EpisodeSetup
  { sSprites :: [SSprite]
  , sBackgroundImage :: SBackground
  , sCustomExtraPrefsOpt :: Maybe CustomExtraPrefs
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data SSprite = SSprite
  { sLabel :: EPeLabel   -- Nome do sprite
  , sPsdPath :: FilePath     -- Caminho do sprite
  , sNumber :: EPeNumber -- Número do sprite (posição no background)
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data SBackground = SBackground
  { bImagePath :: FilePath -- Caminho da imagem de fundo
  , bWidth :: Int -- Largura da imagem
  , bHeight :: Int -- Altura da imagem
  , bSpritePositions :: BSpritePositions -- Posições dos sprites no fundo
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data BSpritePositions
  = SPositionsFor1
    {pFor1Sprite :: PSprite }
  | SPositionsFor2
    { pFor2Sprite1 :: PSprite
    , pFor2Sprite2 :: PSprite }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

bSpritePositionsList :: BSpritePositions -> [PSpriteNumbered]
bSpritePositionsList (SPositionsFor1 sprite) =
  [ PSpriteNumbered sprite EPeNum1 ]
bSpritePositionsList (SPositionsFor2 sprite1 sprite2) =
  [ PSpriteNumbered sprite1 EPeNum1
  , PSpriteNumbered sprite2 EPeNum2]

data PSprite = PSprite
  { sX :: Int -- Posição X do sprite no fundo
  , sY :: Int -- Posição Y do sprite no fundo
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data PSpriteNumbered = PSpriteNumbered
  { snSprite :: PSprite
  , snNumber :: EPeNumber 
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)


exampleEpisodeSetup :: EpisodeSetup
exampleEpisodeSetup = EpisodeSetup
  { sSprites = 
    -- [ SSprite (EPeLabel "pe_felipe777") "02 sprite mega-sushi-temakeria IMPORT.psd" EPeNum1
    [ SSprite (EPeLabel "pe_felipe777") "allianza-consultoria_neto.psd" EPeNum1
    , SSprite (EPeLabel "pe_felipe") "melhores-ofertas.psd" EPeNum2
    ]
  , sBackgroundImage = SBackground
    { bImagePath = "allianza-consultoria-02.jpg"
    , bWidth = 1080
    , bHeight = 1920
    , bSpritePositions = SPositionsFor2
      { pFor2Sprite1 = PSprite (-46) (-13)
      , pFor2Sprite2 = PSprite 46 (-13)
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
      let spritesDir = C.spritesDir conf

      let path = spritesDir </> configPath
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