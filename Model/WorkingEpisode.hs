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
        [ SSprite (EPeLabel "pe_guga") "guga-doces-e-fraldas_mascote-guga.psd" EPeNum1
        , SSprite (EPeLabel "pe_mo") "melhores-ofertas.psd" EPeNum2
        ]
    , sBackgroundImage = SBackground
        { bImagePath = "guga-doces-e-fraldas-17.jpg"
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
  { ePes = [EPeLabel "pe_guga", EPeLabel "pe_mo"]
    , eDialoguePeList =
        [ EDialoguePe
            { dPe = EPeLabel "pe_mo"
            , dContents =
                [ RPlainText "Olá Guga,"
                , RCommand (CPause 300)
                , RPlainText "da Guga Doces e Fraldas!"
                , RCommand (CGesture GHi (EPeLabel "pe_guga"))
                , RCommand (CGesture GHi (EPeLabel "pe_mo"))
                , RCommand (CPause 500)
                , RPlainText "Hoje o papo é sobre 'Cliente Feliz na Festa'!"
                , RCommand (CGesture GThinkShort (EPeLabel "pe_mo"))
                , RCommand (CPause 500)
                , RPlainText "Afinal,"
                , RCommand (CPause 200)
                , RPlainText "vocês se dedicam a isso,"
                , RCommand (CGesture GStandShort (EPeLabel "pe_guga"))
                , RCommand (CPause 500)
                , RPlainText "não é?"
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_guga"
            , dContents =
                [ RPlainText "Com certeza!"
                , RCommand (CGesture GStandLong (EPeLabel "pe_guga"))
                , RCommand (CPause 500)
                , RPlainText "Na nossa loja,"
                , RCommand (CPause 200)
                , RPlainText "a gente ajuda a criar a festa perfeita."
                , RCommand (CGesture GStandShort (EPeLabel "pe_mo"))
                , RCommand (CPause 500)
                , RPlainText "Com uma variedade de doces e artigos de festa,"
                , RCommand (CPause 300)
                , RPlainText "fica muito mais fácil planejar."
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_mo"
            , dContents =
                [ RPlainText "E a variedade é impressionante,"
                , RCommand (CGesture GThinkLong (EPeLabel "pe_mo"))
                , RCommand (CPause 300)
                , RPlainText "desde marshmallows e chocolates,"
                , RCommand (CPause 200)
                , RPlainText "até fraldas e itens de higiene!"
                , RCommand (CPause 500)
                , RPlainText "Tudo num lugar só!"
                , RCommand (CGesture GStandLong (EPeLabel "pe_guga"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_guga"
            , dContents =
                [ RPlainText "Sim,"
                , RCommand (CPause 200)
                , RPlainText "nós temos o compromisso de oferecer produtos de qualidade"
                , RCommand (CPause 500)
                , RPlainText "e um atendimento que realmente faz a diferença."
                , RCommand (CGesture GStandShort (EPeLabel "pe_guga"))
                , RCommand (CPause 300)
                , RPlainText "Nossa equipe está sempre pronta pra dar dicas e orientar na escolha certa."
                , RCommand (CGesture GStandShort (EPeLabel "pe_mo"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_mo"
            , dContents =
                [ RPlainText "É sobre praticidade e confiança,"
                , RCommand (CGesture GStandLong (EPeLabel "pe_mo"))
                , RCommand (CPause 500)
                , RPlainText "pra que o cliente saia com tudo que precisa e mais um pouco."
                , RCommand (CGesture GShakeLegShort (EPeLabel "pe_guga"))
                , RCommand (CPause 500)
                , RPlainText "O objetivo é que cada festa seja um sucesso!"
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_mo"
            , dContents =
                [ RPlainText "E você?"
                , RCommand (CPause 200)
                , RPlainText "Já teve uma experiência bacana com a Guga Doces e Fraldas?"
                , RCommand (CPause 500)
                , RPlainText "Comente abaixo e ajude outros a conhecerem também!"
                , RCommand (CGesture GStandLong (EPeLabel "pe_mo"))
                ]
            }
        ]
    }