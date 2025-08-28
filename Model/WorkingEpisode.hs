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
import Model.EpisodeSetup
import Model.EpisodePersona

import qualified Model.EpisodeComplete as EC
import qualified Model.EpisodeSetup as ES

import qualified Model.AniAutoTask as AAT 

import qualified Model.Config as C

buildEpisodeIO :: IO ()
buildEpisodeIO = do
    config <- C.loadConfigIO
    AAT.prepareWorkingDirIO config episodeSetup
    task <- AAT.episodeCompleteToAniAutoTaskIO myEpisode config
    return ()
    where 
      myEpisode = EC.EpisodeComplete episode episodeSetup

episodeSetup :: EpisodeSetup
episodeSetup = EpisodeSetup
    { sSprites = 
        -- [ SSprite (EPeLabel "pe_felipe777") "02 sprite mega-sushi-temakeria IMPORT.psd" EPeNum1
        [ SSprite (EPeLabel "pe_oseas_couto") "construtora-construcasa_oseas-couto.psd" EPeNum1
        , SSprite (EPeLabel "pe_melhores_ofertas") "melhores-ofertas.psd" EPeNum2
        ]
    , sBackgroundImage = SBackground
        { bImagePath = "construtora-construcasa-07.jpg"
        , bWidth = 1080
        , bHeight = 1920
        , bSpritePositions = SPositionsFor2
            { pFor2Sprite1 = PSprite (-46) (-13)
            , pFor2Sprite2 = PSprite 46 (-13)
            }
        }
    }

episode :: Episode
episode = Episode
    { ePes = [EPeLabel "pe_oseas_couto", EPeLabel "pe_melhores_ofertas"]
    , eDialoguePeList =
        [ EDialoguePe
            { dPe = EPeLabel "pe_melhores_ofertas"
            , dContents =
                [ RPlainText "Olá, Oséas da Construtora Construcasa!"
                , RCommand (CGesture GHi (EPeLabel "pe_melhores_ofertas"))
                , RCommand (CPause 500)
                , RPlainText "Hoje vamos falar sobre as soluções completas que a sua empresa oferece."
                , RCommand (CGesture GTalkShort (EPeLabel "pe_melhores_ofertas"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_oseas_couto"
            , dContents =
                [ RPlainText "Exatamente! Da construção sob medida até a venda de terrenos e imóveis, estamos aqui para transformar sonhos em realidade."
                , RCommand (CGesture GTalkLong (EPeLabel "pe_oseas_couto"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_melhores_ofertas"
            , dContents =
                [ RPlainText "E não para por aí, né? A Construcasa também administra obras e facilita financiamentos para a casa própria!"
                , RCommand (CGesture GTalkShort (EPeLabel "pe_melhores_ofertas"))
                , RCommand (CGesture GStandShort (EPeLabel "pe_oseas_couto"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_melhores_ofertas"
            , dContents =
                [ RCommand (CPause 1000)
                , RPlainText "Se você também ama os serviços da Construtora Construcasa, deixe seu comentário aqui e ajude outros a descobrirem também!"
                , RCommand (CGesture GStandShort (EPeLabel "pe_melhores_ofertas"))
                ]
            }
        ]
    } 