{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- MyModule.hs
module Model.AudiosInfo where

import GHC.Generics (Generic) 

import qualified Data.Text as T
import Data.Text (Text)

import qualified Data.ByteString.Lazy.Char8 as BL

import Model.GuidoLangUtil (GLScript(GLAudiosInfo), glCall)

import Data.Aeson (ToJSON, FromJSON)

-- Utiliza para comunicar com o GuidoLang, parâmetro de envio
newtype AudiosRequest = AudiosRequest [AudioRequest] 
    deriving (Show, Eq, Generic, ToJSON)

-- Utiliza para comunicar com o GuidoLang, resultado
newtype AudiosInfo = AudiosInfo [AudioInfo]
    deriving (Show, Eq, Generic, FromJSON)

data AudioInfo = AudioInfo
    -- Caminho do arquivo de áudio, pode ser um número 00001.wav, 00002.wav, etc.
    { aiFilePath :: FilePath 
    -- Duração do áudio em milissegundos
    , aiDuration :: Int 
    , aiText :: Text -- Texto original que foi convertido em áudio
    } deriving (Show, Eq, Generic, FromJSON)

data AudioRequest = AudioRequest
    { arText :: Text -- Texto a ser convertido em áudio
    , arConfig :: AudioRequestConfig -- Configurações do áudio
    } deriving (Show, Eq, Generic, ToJSON)

data AudioRequestConfig = AudioRequestConfig
    { arcVoice :: Text -- Voz a ser utilizada, ex: pt-BR-AntonioNeural
    , arcPitch :: Text -- "+0Hz", Exemplo: "+5Hz" para aumentar o tom
    , arcRate :: Text -- "+0%", Exemplo: "+5Hz" para aumentar o tom
    , arcTimbreScale :: Double -- Padrão 1.0, maior ou igual 0.5, menor ou igual 1.5, fator de variação do timbre
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

defaultAudioRequestConfig :: AudioRequestConfig
defaultAudioRequestConfig = AudioRequestConfig
    { arcVoice = "pt-BR-AntonioNeural"
    , arcPitch = "+0Hz"
    , arcRate = "+0%"
    , arcTimbreScale = 1.0
    }

-- Função para solicitar áudios ao GuidoLang 
requestAudiosIO :: AudiosRequest -> IO AudiosInfo
requestAudiosIO = glCall GLAudiosInfo