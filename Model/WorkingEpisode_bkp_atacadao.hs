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
        [ SSprite (EPeLabel "pe_fabiana") "atacadao-dos-pisos.psd" EPeNum1
        , SSprite (EPeLabel "pe_mo") "melhores-ofertas.psd" EPeNum2
        ]
    , sBackgroundImage = SBackground
        { bImagePath = "atacadao-dos-pisos-03.jpg"
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
    { ePes = [EPeLabel "pe_fabiana", EPeLabel "pe_mo"]
    , eDialoguePeList = 
        [ EDialoguePe
            { dPe = EPeLabel "pe_mo"
            , dContents = 
                [ RPlainText "Olá, eu sou o Melhores Ofertas e vim conversar com a Fabiana, do Atacadão dos Pisos Itapira!"
                , RCommand (CGesture GHi (EPeLabel "pe_mo"))
                , RCommand (CPause 500)
                , RPlainText "Fabiana, qual o grande diferencial da sua loja?"
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_fabiana"
            , dContents =
                [ RCommand (CGesture GStandShort (EPeLabel "pe_fabiana"))
                , RPlainText "Com certeza, a nossa variedade! Temos pisos cerâmicos e porcelanatos de diversos tamanhos, cores e texturas. Do clássico ao moderno, tudo em um só lugar!"
                , RCommand (CGesture GTalkLong (EPeLabel "pe_fabiana"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_mo"
            , dContents =
                [ RPlainText "É muita opção pra quem quer reformar ou construir. E o melhor é que tem o preço do atacado!"
                , RCommand (CGesture GExcited (EPeLabel "pe_mo"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_fabiana"
            , dContents =
                [ RPlainText "Isso mesmo! Temos um mix completo para todos os estilos!"
                , RCommand (CGesture GTalkShort (EPeLabel "pe_fabiana"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_mo"
            , dContents =
                [ RPlainText "Já conhece o Atacadão dos Pisos? Conta pra gente nos comentários como foi sua experiência!"
                , RCommand (CGesture GTalkShort (EPeLabel "pe_mo"))
                ]
            }
        ]
    }