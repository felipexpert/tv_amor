{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- MyModule.hs
module Model.AudiosInfo where

-- Utiliza para comunicar com o GuidoLang, parâmetro de envio
newtype AudiosRequest = [AudioRequest] deriving (Show, Eq, Generic)

-- Utiliza para comunicar com o GuidoLang, resultado
newtype AudiosInfo = [AudioInfo] deriving (Show, Eq, Generic)

data AudioInfo = AudioInfo
    -- Caminho do arquivo de áudio, pode ser um número 00001.wav, 00002.wav, etc.
    { aiFilePath :: FilePath 
    -- Duração do áudio em milissegundos
    , aiDuration :: Int 
    } deriving (Show, Eq, Generic)

data AudioRequest = AudioRequest
    { arText :: Text -- Texto a ser convertido em áudio
    , arConfig :: AudioRequestConfig -- Configurações do áudio
    } deriving (Show, Eq, Generic)

data AudioRequestConfig = AudioRequestConfig
    { arcVoice :: Text -- Voz a ser utilizada
    -- vai ter mais campos para parâmetros do timbre da voz
    } deriving (Show, Eq, Generic)

-- Função para solicitar áudios ao GuidoLang
requestAudiosIO :: AudiosRequest -> IO AudiosInfo
requestAudiosIO ar = do 
    -- Aqui você implementaria a lógica para enviar a requisição ao GuidoLang
    -- e receber a resposta com as informações dos áudios.
    
    