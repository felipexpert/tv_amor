{-# LANGUAGE OverloadedStrings #-}

module Model.WorkingEpisode where

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

import Model.Episode
import Model.EpisodePersona

import qualified Model.EpisodeComplete as EC
import qualified Model.EpisodeSetup as ES

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

buildEpisodeIO :: IO ()
buildEpisodeIO = do
    config <- C.loadConfigIO
    AAT.prepareWorkingDirIO config setup
    task <- AAT.episodeCompleteToAniAutoTaskIO myEpisode config
    return ()
    where 
      myEpisode = EC.EpisodeComplete episode01 setup
      setup = ES.exampleEpisodeSetup

episode01 :: Episode
episode01 = Episode
    { ePes = [EPeLabel "pe_felipe", EPeLabel "pe_felipe777"]
    , eDialoguePeList =
        [ EDialoguePe
            { dPe = EPeLabel "pe_felipe777"
            , dContents =
                [ RPlainText "Fala, pessoal! Aqui é o Melhores Ofertas!"
                , RCommand (CGesture GHi (EPeLabel "pe_felipe777"))
                , RCommand (CPause 200)
                , RPlainText "Hoje eu tô aqui com o Neto, da Allianza Consultoria."
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_felipe"
            , dContents =
                [ RCommand (CGesture GTalkShort (EPeLabel "pe_felipe"))
                , RPlainText "Fala, Melhores Ofertas! É um prazer estar aqui!"
                , RCommand (CGesture GHi (EPeLabel "pe_felipe"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_felipe777"
            , dContents =
                [ RPlainText "Neto, me diz aí, quais as vantagens de ter uma administradora de condomínios?"
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_felipe"
            , dContents =
                [ RCommand (CGesture GTalkLong (EPeLabel "pe_felipe"))
                , RPlainText "Terceirizar a gestão financeira e burocrática otimiza a rotina dos síndicos e melhora a qualidade de vida dos moradores!"
                , RCommand (CPause 500)
                , RPlainText "Sem falar que garante mais transparência e segurança para o condomínio."
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_felipe777"
            , dContents =
                [ RCommand (CGesture GThinkShort (EPeLabel "pe_felipe777"))
                , RPlainText "A Allianza Consultoria é a solução ideal pra quem busca eficiência e tranquilidade."
                , RCommand (CPause 200)
                , RPlainText "Se você já conhece a Allianza, conta pra gente nos comentários como foi sua experiência!"
                , RCommand (CGesture GExcited (EPeLabel "pe_felipe777"))
                ]
            }
        ]
    }