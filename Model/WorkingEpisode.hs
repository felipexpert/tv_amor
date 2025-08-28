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
        [ SSprite (EPeLabel "Jean") "espetinho-e-cia_jean.psd" EPeNum1
        , SSprite (EPeLabel "Melhores Ofertas") "melhores-ofertas.psd" EPeNum2
        ]
    , sBackgroundImage = SBackground
        { bImagePath = "espetinho-e-cia-09.jpg"
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
    { ePes = [EPeLabel "Jean", EPeLabel "Melhores Ofertas"]
    , eDialoguePeList =
        [ EDialoguePe
            { dPe = EPeLabel "Melhores Ofertas"
            , dContents =
                [ RPlainText "Olá pessoal!"
                , RCommand (CGesture GHi (EPeLabel "Melhores Ofertas"))
                , RCommand (CPause 500)
                , RPlainText "Hoje estamos aqui com o Jean da Espetinho & Cia em Itapira!"
                , RCommand (CGesture GTalkShort (EPeLabel "Melhores Ofertas"))
                , RCommand (CGesture GHi (EPeLabel "Jean"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "Jean"
            , dContents =
                [ RPlainText "É isso aí!"
                , RCommand (CGesture GExcited (EPeLabel "Jean"))
                , RCommand (CPause 500)
                , RPlainText "Vamos falar de uma combinação que todo mundo ama. Churrasco e Chopp!"
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "Melhores Ofertas"
            , dContents =
                [ RPlainText "Combinação perfeita, Jean!"
                , RCommand (CGesture GTalkShort (EPeLabel "Melhores Ofertas"))
                , RPlainText "Aqui na Espetinho & Cia, as pessoas encontram tudo para o churrasco, não é mesmo?"
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "Jean"
            , dContents =
                [ RPlainText "Exatamente!"
                , RCommand (CPause 500)
                , RPlainText "Temos espetos de todos os tipos, kafta, linguiça, medalhões e o famoso queijo coalho."
                , RCommand (CGesture GTalkLong (EPeLabel "Jean"))
                , RCommand (CPause 250)
                , RPlainText "E claro, o chopp geladinho para acompanhar!"
                , RCommand (CGesture GExcited (EPeLabel "Jean"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "Melhores Ofertas"
            , dContents =
                [ RPlainText "É de dar água na boca!"
                , RCommand (CGesture GThinkShort (EPeLabel "Melhores Ofertas"))
                , RPlainText "A Espetinho & Cia fica na Avenida dos Italianos, 160, no centro de Itapira."
                , RCommand (CPause 500)
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "Melhores Ofertas"
            , dContents =
                [ RPlainText "Se você também ama os produtos da Espetinho & Cia,."
                , RCommand (CPause 500)
                , RPlainText "deixe seu comentário aqui e ajude outros a descobrirem também!"
                ]
            }
        ]
    }