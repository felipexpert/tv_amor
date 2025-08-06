{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- MyModule.hs
module Model.Episode where

import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics (Generic) 

import Model.EpisodePersona (EPeLabel(..))


-- O episódio completo (lista de blocos)
-- NOTA: Posso fazer validação, aceita 1 ou 2 personas, e no eDialogueBlock, aceita apenas
-- EPeLabel que estão na lista de ePes
data Episode = Episode 
    { ePes :: [EPeLabel] -- Lista de personagens que participam do episódio
    , eDialoguePeList :: [EDialoguePe] -- Lista de blocos de fala
    } deriving (Show, Eq, Generic)


-- Um bloco de fala, associado a um personagem
data EDialoguePe = EDialoguePe
    { dPe :: EPeLabel
    , dContents :: [DRichText]
    } deriving (Show, Eq)

-- Um trecho de texto com comandos embutidos
data DRichText
    = RPlainText Text
    | RCommand RCCommand
    deriving (Show, Eq)

-- Representa os comandos embutidos no texto
data RCCommand 
    -- ERTCGesture tem quem está fazendo o gesto, porque durante a fala de um, o outro pode fazer um gesto
    = CGesture
        { cGesture :: CGesture
        , cPe :: EPeLabel }
    | CPause Int -- Pausa em milissegundos
    deriving (Show, Eq)

data CGesture
    = GWave
    | GThink1 -- detalhes como duração, podem variar, por isso tem EGThing1 e EGThink2, etc
    | GThink2
    deriving (Show, Eq) 

exampleEpisode :: Episode
exampleEpisode = Episode
    { ePes = [EPeLabel "pe_felipe", EPeLabel "pe_gisele"]
    , eDialoguePeList = exampleDialogues
    }
    where
        exampleDialogues :: [EDialoguePe]
        exampleDialogues =
            [ EDialoguePe
                { dPe = EPeLabel "pe_felipe"
                , dContents =
                    [ RPlainText "Olá Gisele"
                    , RCommand (CGesture GWave (EPeLabel "pe_felipe"))
                    , RCommand (CGesture GWave (EPeLabel "pe_gisele"))
                    , RCommand (CPause 500)
                    , RPlainText "Tudo bem por aí?"
                    ]
                }
            , EDialoguePe
                { dPe = EPeLabel "pe_gisele"
                , dContents =
                    [ RPlainText "Olá Felipe! Tudo ótimo!" ]
                }
            ]