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
        [ SSprite (EPeLabel "pe_silmara") "silmara.psd" EPeNum1
        , SSprite (EPeLabel "pe_melhores_ofertas") "melhores-ofertas.psd" EPeNum2
        ]
    , sBackgroundImage = SBackground
        { bImagePath = "silmara-15.jpg"
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
    { ePes = [EPeLabel "pe_silmara", EPeLabel "pe_melhores_ofertas"]
    , eDialoguePeList =
        [ EDialoguePe
            { dPe = EPeLabel "pe_melhores_ofertas"
            , dContents =
                [ RPlainText "Olá! Sou o Melhores Ofertas, e hoje conversamos com a Psicóloga e Neuropsicóloga Silmara Cristina Luciano."
                , RCommand (CGesture GHi (EPeLabel "pe_melhores_ofertas"))
                , RCommand (CGesture GHi (EPeLabel "pe_silmara"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_silmara"
            , dContents =
                [ RPlainText "Olá! É um prazer estar aqui."
                , RCommand (CGesture GStandShort (EPeLabel "pe_silmara"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_melhores_ofertas"
            , dContents =
                [ RPlainText "Silmara, o assunto de hoje é: O que é Avaliação Neuropsicológica e para quem ela serve?"
                , RCommand (CGesture GTalkShort (EPeLabel "pe_melhores_ofertas"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_silmara"
            , dContents =
                [ RPlainText "Ótima pergunta! A Avaliação Neuropsicológica é um exame detalhado que investiga como o cérebro funciona." , RCommand (CPause 500) , RPlainText " Ela avalia habilidades como memória, atenção e linguagem." , RCommand (CGesture GThinkShort (EPeLabel "pe_melhores_ofertas"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_melhores_ofertas"
            , dContents =
                [ RPlainText "Entendi. E para quem ela é indicada?"
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_silmara"
            , dContents =
                [ RPlainText "Serve para todas as idades!" , RCommand (CGesture GExcited (EPeLabel "pe_silmara")) , RCommand (CPause 300) , RPlainText " Desde crianças com dificuldades de aprendizagem, até adultos com suspeita de T.D.A.H. ou idosos com queixas de memória."
                , RCommand (CGesture GTalkLong (EPeLabel "pe_silmara"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_melhores_ofertas"
            , dContents =
                [ RPlainText "Mostre o seu apoio! Comente elogiando a Psicóloga e Neuropsicóloga Silmara Cristina Luciano!"
                ]
            }
        ]
    }