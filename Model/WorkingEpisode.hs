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
        [ SSprite (EPeLabel "Jean") "espetinho-e-cia_jean.psd" EPeNum1
        , SSprite (EPeLabel "MO") "melhores-ofertas.psd" EPeNum2
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
    { ePes = [EPeLabel "Jean", EPeLabel "MO"]
    , eDialoguePeList =
        [ EDialoguePe
            { dPe = EPeLabel "MO"
            , dContents =
                [ RPlainText "Olá"
                , RCommand (CGesture GHi (EPeLabel "MO"))
                , RCommand (CPause 5000)
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "Jean"
            , dContents =
                [ RPlainText "Stand Short"
                , RCommand (CGesture GStandShort (EPeLabel "Jean"))
                , RCommand (CPause 5000)
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "MO"
            , dContents =
                [ RPlainText "Stand Long"
                , RCommand (CGesture GStandLong (EPeLabel "MO"))
                , RCommand (CPause 5000)
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "Jean"
            , dContents =
                [ RPlainText "Think Short"
                , RCommand (CGesture GThinkShort (EPeLabel "Jean"))
                , RCommand (CPause 5000)
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "MO"
            , dContents =
                [ RPlainText "Think Long"
                , RCommand (CGesture GThinkLong (EPeLabel "MO"))
                , RCommand (CPause 5000)
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "Jean"
            , dContents =
                [ RPlainText "Worry Short"
                , RCommand (CGesture GWorryShort (EPeLabel "Jean"))
                , RCommand (CPause 5000)
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "MO"
            , dContents =
                [ RPlainText "Worry Long"
                , RCommand (CGesture GWorryLong (EPeLabel "MO"))
                , RCommand (CPause 5000)
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "Jean"
            , dContents =
                [ RPlainText "Shake Leg Short"
                , RCommand (CGesture GShakeLegShort (EPeLabel "Jean"))
                , RCommand (CPause 5000)
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "MO"
            , dContents =
                [ RPlainText "Shake Leg Long"
                , RCommand (CGesture GShakeLegLong (EPeLabel "MO"))
                , RCommand (CPause 5000)
                ]
            }

        ]
    }