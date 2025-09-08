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
        -- [ SSprite (EPeLabel "pe_felipe777") "02 sprite mega-sushi-temakeria IMPORT.psd" EPeNum1
        [ SSprite (EPeLabel "pe_amanda") "cacau-show-itapira_amanda.psd" EPeNum1
        , SSprite (EPeLabel "pe_mo") "melhores-ofertas.psd" EPeNum2
        ]
    , sBackgroundImage = SBackground
        { bImagePath = "cacau-show-itapira-04.jpg"
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
    { ePes = [EPeLabel "pe_amanda", EPeLabel "pe_mo"]
    , eDialoguePeList =
        [ EDialoguePe
            { dPe = EPeLabel "pe_mo"
            , dContents =
                [ RPlainText "Olá, Amanda da Cacau Show Itapira!"
                , RCommand (CGesture GHi (EPeLabel "pe_mo"))
                , RPlainText "Hoje vamos falar da tradição e qualidade da sua loja!"
                , RCommand (CGesture GTalkShort (EPeLabel "pe_mo"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_amanda"
            , dContents =
                [ RPlainText "Exato!"
                , RCommand (CGesture GExcited (EPeLabel "pe_amanda"))
                , RPlainText "Nossos chocolates são conhecidos por serem os melhores."
                , RCommand (CGesture GTalkShort (EPeLabel "pe_amanda"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_mo"
            , dContents =
                [ RPlainText "Uma qualidade que o cliente conhece e confia, né?"
                , RCommand (CGesture GThinkShort (EPeLabel "pe_mo"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_amanda"
            , dContents =
                [ RPlainText "Sim, e isso a gente entrega todos os dias!"
                , RCommand (CGesture GTalkShort (EPeLabel "pe_amanda"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_mo"
            , dContents =
                [ RPlainText "Já teve uma experiência bacana com a Cacau Show Itapira?"
                , RCommand (CGesture GStandShort (EPeLabel "pe_mo"))
                , RPlainText "Comente abaixo e inspire outros clientes a conhecerem também!"
                ]
            }
        ]
    }