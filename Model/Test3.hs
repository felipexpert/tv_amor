{-# LANGUAGE OverloadedStrings #-}

module Model.Test3 where

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

import qualified Model.EpisodeComplete as EC

import qualified Model.AniAutoTask as AAT 

import qualified Model.Config as C

{-
testIO :: IO ()
testIO = do
    let audioRequest = AudioRequest "Olá, humanos!" (AudioRequestConfig "pt-BR-AntonioNeural")
        audioRequest2 = AudioRequest "Tudo bem?!" (AudioRequestConfig "pt-BR-AntonioNeural")
        audiosRequest = AudiosRequest [audioRequest, audioRequest2]
    
    -- Envia o pedido de áudio e recebe a resposta
    TIO.putStrLn "Enviando pedido de áudio ao GuidoLang..."
    -- audiosInfo <- glCall GLAudiosInfoTest audiosRequest
    audiosInfo <- glCall GLAudiosInfo audiosRequest
    
    let audiosInfo' = audiosInfo :: AudiosInfo

    -- Imprime as informações dos áudios recebidos
    TIO.putStrLn "Áudios recebidos:"
    TU.putShowable audiosInfo'
-}

-- episodeCompleteToAniAutoTaskIO :: EpisodeComplete -> IO AniAutoTask

testIO :: IO ()
testIO = do
    config <- C.loadConfigIO
    AAT.prepareWorkingDirIO config exampleSetup
    task <- AAT.episodeCompleteToAniAutoTaskIO example
    return ()
    where 
        example = EC.exampleEpisodeComplete
        exampleSetup = EC.exampleEpisodeSetup example
