{-# LANGUAGE OverloadedStrings #-}

module Model.Test2 where

import System.Process
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import Control.Monad (void)
import System.IO (hClose)

import qualified Data.Text.IO as TIO
import qualified Model.TextUtil as TU

import Model.AudiosInfo
import Model.GuidoLangUtil

testIO :: IO ()
testIO = do
    let audioRequest = AudioRequest "Olá, humanos!" defaultAudioRequestConfig
        audioRequest2 = AudioRequest "Tudo bem?!" defaultAudioRequestConfig
        audiosRequest = AudiosRequest [audioRequest, audioRequest2]
    
    -- Envia o pedido de áudio e recebe a resposta
    TIO.putStrLn "Enviando pedido de áudio ao GuidoLang..."
    -- audiosInfo <- glCall GLAudiosInfoTest audiosRequest
    audiosInfo <- glCall GLAudiosInfo audiosRequest
    
    let audiosInfo' = audiosInfo :: AudiosInfo

    -- Imprime as informações dos áudios recebidos
    TIO.putStrLn "Áudios recebidos:"
    TU.putShowable audiosInfo'

