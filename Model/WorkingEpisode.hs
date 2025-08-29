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
        [ SSprite (EPeLabel "pe_damiao") "mega-sushi-temakeria.psd" EPeNum1
        , SSprite (EPeLabel "pe_melhores_ofertas") "melhores-ofertas.psd" EPeNum2
        ]
    , sBackgroundImage = SBackground
        { bImagePath = "mega-sushi-temakeria-12.jpg"
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
    { ePes = [EPeLabel "pe_damiao", EPeLabel "pe_melhores_ofertas"]
    , eDialoguePeList =
        [ EDialoguePe
            { dPe = EPeLabel "pe_melhores_ofertas"
            , dContents =
                [ RPlainText "Olá, pessoal!"
                , RCommand (CGesture GHi (EPeLabel "pe_melhores_ofertas"))
                , RCommand (CGesture GHi (EPeLabel "pe_damiao"))
                , RCommand (CPause 500)
                , RPlainText "Hoje estou aqui com o Damião, do Mega Sushi Temakeria de Paulínia!"
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_damiao"
            , dContents =
                [ RPlainText "É isso aí!"
                , RCommand (CGesture GTalkShort (EPeLabel "pe_damiao"))
                , RCommand (CPause 500)
                , RPlainText "E o nosso papo hoje é sobre como a gente tem opções para a família inteira!"
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_melhores_ofertas"
            , dContents =
                [ RPlainText "Exatamente!"
                , RCommand (CGesture GThinkShort (EPeLabel "pe_melhores_ofertas"))
                , RCommand (CPause 500)
				, RPlainText "Porque a gente sabe que as pessoas na família têm diferentes paladares, não é Damião?"
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_damiao"
            , dContents =
                [ RPlainText "É verdade!"
                , RCommand (CGesture GExcited (EPeLabel "pe_damiao"))
                , RCommand (CPause 500)
                , RPlainText "Por isso, além dos nossos famosos temakis, temos Yakissobas e até porções como batata com queijo!"
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_melhores_ofertas"
            , dContents =
                [ RPlainText "Ou seja, o pai pede um combinado de salmão,"
                , RCommand (CPause 500)
                , RPlainText "enquanto a mãe escolhe o hot holl e o filho se delicia com um yakisoba!"
                , RCommand (CGesture GTalkLong (EPeLabel "pe_melhores_ofertas"))
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_damiao"
            , dContents =
                [ RPlainText "É sabor e alegria pra todas as pessoas,"
                , RCommand (CGesture GStandShort (EPeLabel "pe_damiao"))
                , RCommand (CPause 500)
                , RPlainText "ninguém fica de fora!"
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_melhores_ofertas"
            , dContents =
                [ RPlainText "Já teve uma experiência bacana com a Mega Sushi Temakeria?"
                , RCommand (CGesture GStandLong (EPeLabel "pe_melhores_ofertas"))
                , RCommand (CPause 500)
                , RPlainText "Comente abaixo e inspire outros clientes a conhecerem também!"
                ]
            }
        ]
    }