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
        [ SSprite (EPeLabel "pe_luis") "casa-vermelha_luis.psd" EPeNum1
        , SSprite (EPeLabel "pe_mo") "melhores-ofertas.psd" EPeNum2
        ]
    , sBackgroundImage = SBackground
        { bImagePath = "casa-vermelha-05.jpg"
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
    { ePes = [EPeLabel "pe_luis", EPeLabel "pe_mo"]
    , eDialoguePeList =
        [ EDialoguePe
            { dPe = EPeLabel "pe_mo"
            , dContents =
                [ RPlainText "Olá, Luís da Casa Vermelha! Tudo pronto para falar do que realmente importa para seus clientes?"
                , RCommand (CGesture GHi (EPeLabel "pe_mo"))
                , RCommand (CPause 500)
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_luis"
            , dContents =
                [ RCommand (CGesture GTalkShort (EPeLabel "pe_luis"))
                , RPlainText "Olá! Com certeza! Estamos focados em mostrar que aqui a moda masculina que encontra você, oferecendo estilo e qualidade para todos."
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_mo"
            , dContents =
                [ RCommand (CGesture GTalkShort (EPeLabel "pe_mo"))
                , RPlainText "Perfeito! Então vamos destacar as camisas, calças de sarja, jeans e os sapatênis que são os mais procurados pelos homens de Itapira e região!"
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_luis"
            , dContents =
                [ RCommand (CGesture GStandShort (EPeLabel "pe_luis"))
                , RPlainText "Isso mesmo! E não podemos esquecer dos looks plus size, que mostram que moda é para todas as pessoas."
                ]
            }
        , EDialoguePe
            { dPe = EPeLabel "pe_mo"
            , dContents =
                [ RCommand (CPause 500)
                , RPlainText "Já conhece a Casa Vermelha? Conta pra gente nos comentários como foi a sua experiência!"
                ]
            }
        ]
    }