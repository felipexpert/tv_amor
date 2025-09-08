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
      myEpisode = EC.EpisodeComplete episode' episodeSetup
      episode' = addPauseIfNeeded episode

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
                [ RPlainText "E aí, Guga! "
                , RCommand (CGesture GStandShort (EPeLabel "pe_mo"))
                , RPlainText "Beleza? "
                , RCommand (CGesture GStandShort (EPeLabel "pe_guga"))
                , RPlainText "Melhores Ofertas aqui, com o Guga da Guga Doces e Fraldas! "
                , RPlainText "Hoje, a gente vai falar daquele momento que todo mundo ama: o \"Cliente Feliz na Festa!\""
                , RCommand (CGesture GThinkShort (EPeLabel "pe_guga"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_guga"
            , dContents =
                [ RPlainText "É isso mesmo, Melhores Ofertas! "
                , RCommand (CGesture GStandShort (EPeLabel "pe_guga"))
                , RPlainText "A gente sabe que festa de criança é uma bagunça deliciosa, mas também é um desafio, né? "
                , RPlainText "Principalmente pra manter todo mundo satisfeito e o aniversariante feliz. "
                , RCommand (CGesture GThinkLong (EPeLabel "pe_mo"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_mo"
            , dContents =
                [ RPlainText "Exatamente! "
                , RCommand (CGesture GStandShort (EPeLabel "pe_mo"))
                , RPlainText "E é aí que os doces e os kits de festa da Guga fazem toda a diferença!"
                , RCommand (CGesture GShakeLegShort (EPeLabel "pe_guga"))
                , RPlainText "Imagina a alegria do cliente em ver a festa do filho com produtos de qualidade, "
                , RCommand (CPause 200)
                , RPlainText "feitos com carinho. "
                , RPlainText "Isso não tem preço!"
                , RCommand (CGesture GStandShort (EPeLabel "pe_guga"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_guga"
            , dContents =
                [ RPlainText "Sem falar da praticidade, né? "
                , RCommand (CGesture GStandShort (EPeLabel "pe_guga"))
                , RPlainText "O cliente não tem que se preocupar com nada, "
                , RCommand (CPause 200)
                , RPlainText "é só fazer o pedido, receber e aproveitar a festa! "
                , RCommand (CGesture GStandShort (EPeLabel "pe_mo"))
                , RPlainText "Isso que é cliente feliz!"
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_mo"
            , dContents =
                [ RPlainText "Queremos saber: qual foi a sua experiência com a Guga Doces e Fraldas?"
                , RCommand (CPause 200)
                , RPlainText "Deixe seu comentário e inspire os outros clientes!"
                ]
            }
        ]
    }