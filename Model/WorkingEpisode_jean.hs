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
        [ SSprite (EPeLabel "Fernando") "farmacia-sao-jose_fernando.psd" EPeNum1
        , SSprite (EPeLabel "Melhores Ofertas") "melhores-ofertas.psd" EPeNum2
        ]
    , sBackgroundImage = SBackground
        { bImagePath = "farmacia-sao-jose-10.jpg"
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
    { ePes = [EPeLabel "Fernando", EPeLabel "Melhores Ofertas"]
    , eDialoguePeList =
        [ EDialoguePe
            { dPe = EPeLabel "Melhores Ofertas"
            , dContents =
                [ RPlainText "Olá, pessoal!"
                , RCommand (CPause 500)
                , RPlainText "Hoje estou aqui com o Representante da Farmácia São José de Itapira."
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "Fernando"
            , dContents =
                [ RPlainText "Isso mesmo!"
                , RCommand (CPause 500)
                , RPlainText "E hoje vamos falar sobre algo que é a nossa essência: o atendimento humano e personalizado."
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "Melhores Ofertas"
            , dContents =
                [ RPlainText "Exatamente! Em uma sociedade cada vez mais digital,"
                , RCommand (CPause 300)
                , RPlainText "aquele cuidado próximo e atencioso faz toda a diferença, não é verdade?"
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "Fernando"
            , dContents =
                [ RPlainText "Com certeza! Para nós da Farmácia São José, com mais de 40 anos de história,"
                , RCommand (CPause 300)
                , RPlainText "cada cliente é um amigo que conhecemos pelo nome."
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "Melhores Ofertas"
            , dContents =
                [ RPlainText "Já teve uma experiência bacana com a Farmácia São José? Comente abaixo e inspire outros clientes a conhecerem também!"
                ]
            }
        ]
    }