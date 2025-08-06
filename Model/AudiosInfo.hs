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
    } deriving (Show, Eq, Generic, FromJSON)

data AudioRequest = AudioRequest
    { arText :: Text -- Texto a ser convertido em áudio
    , arConfig :: AudioRequestConfig -- Configurações do áudio
    } deriving (Show, Eq, Generic, ToJSON)

data AudioRequestConfig = AudioRequestConfig
    { arcVoice :: Text -- Voz a ser utilizada
    -- vai ter mais campos para parâmetros do timbre da voz
    } deriving (Show, Eq, Generic, ToJSON)

-- Função para solicitar áudios ao GuidoLang 
requestAudiosIO :: AudiosRequest -> IO AudiosInfo
requestAudiosIO = glCall GLAudiosInfo