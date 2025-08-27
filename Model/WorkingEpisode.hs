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
        [ SSprite (EPeLabel "pe_felipe777") "allianza-consultoria_neto.psd" EPeNum1
        , SSprite (EPeLabel "pe_felipe") "melhores-ofertas.psd" EPeNum2
        ]
    , sBackgroundImage = SBackground
        { bImagePath = "allianza-consultoria-02.jpg"
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
    { ePes = [ EPeLabel "pe_felipe777", EPeLabel "pe_felipe"]
    , eDialoguePeList =
        [ EDialoguePe
            { dPe = EPeLabel "pe_felipe"
            , dContents =
                [ RPlainText "Fala, pessoal! Aqui é o Melhores Ofertas!"
                , RCommand (CGesture GHi (EPeLabel "pe_felipe"))
                , RCommand (CPause 200)
                , RPlainText "Hoje eu tô aqui com o Neto, da Allianza Consultoria."
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_felipe777"
            , dContents =
                [ RCommand (CGesture GTalkShort (EPeLabel "pe_felipe777"))
                , RPlainText "Fala, Melhores Ofertas! É um prazer estar aqui!"
                , RCommand (CGesture GHi (EPeLabel "pe_felipe777"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_felipe"
            , dContents =
                [ RPlainText "Neto, me diz aí, quais as vantagens de ter uma administradora de condomínios?"
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_felipe777"
            , dContents =
                [ RCommand (CGesture GTalkLong (EPeLabel "pe_felipe777"))
                , RPlainText "Terceirizar a gestão financeira e burocrática otimiza a rotina dos síndicos e melhora a qualidade de vida dos moradores!"
                , RCommand (CPause 500)
                , RPlainText "Sem falar que garante mais transparência e segurança para o condomínio."
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_felipe"
            , dContents =
                [ RCommand (CGesture GThinkShort (EPeLabel "pe_felipe"))
                , RPlainText "A Allianza Consultoria é a solução ideal pra quem busca eficiência e tranquilidade."
                , RCommand (CPause 200)
                , RPlainText "Se você já conhece a Allianza, conta pra gente nos comentários como foi sua experiência!"
                , RCommand (CGesture GExcited (EPeLabel "pe_felipe"))
                ]
            }
        ]
    }
