{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- MyModule.hs
module Model.Episode where

import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics (Generic) 

import Model.EpisodePersona (EPeLabel(..), EPeNumber(..))

import qualified Data.List as List

import Data.Aeson (ToJSON, FromJSON)


-- O episódio completo (lista de blocos)
-- NOTA: Posso fazer validação, aceita 1 ou 2 personas, e no eDialogueBlock, aceita apenas
-- EPeLabel que estão na lista de ePes
data Episode = Episode 
    { ePes :: [EPeLabel] -- Lista de personagens que participam do episódio
    , eDialoguePeList :: [EDialoguePe] -- Lista de blocos de fala
    } deriving (Show, Eq, Generic)

episodePeNumber :: Episode -> EPeLabel -> EPeNumber
episodePeNumber episode label = peNumber
    where 
        indexOpt :: Maybe Int
        indexOpt = List.findIndex sameLabel (ePes episode)
            where 
                sameLabel :: EPeLabel -> Bool
                sameLabel = (== label) 
        peNumber :: EPeNumber
        peNumber = case indexOpt of
            Just 0 -> EPeNum1
            Just 1 -> EPeNum2
            _      -> EPeNum1 -- Default, caso não encontre, retorna EPeNum1

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
    = GHi -- Aceno
    | GStandShort -- Fica com as mãos juntas, depois volta as mãos
    | GStandLong -- Fica com as mãos juntas por mais tempo, depois volta as mãos
    | GThinkShort -- Coloca a mão no queixo, reflexivo
    | GThinkLong -- Coloca a mão no queixo, reflexivo, por mais tempo
    | GTalkShort -- movimenta os braços, expressando ideias
    | GTalkLong -- movimenta os braços, expressando ideias, por mais tempo
    | GWorryShort -- demonstra um pouco de preocupação
    | GWorryLong -- demonstra um pouco de preocupação, por mais tempo
    | GShakeLegShort -- balança um pouco a perna
    | GShakeLegLong -- balança um pouco a perna, por mais tempo
    | GExcited -- fica animado!
    | GDance -- Dança de alegria (um pouco exagerado, não utilize tanto esse)
    | GDefault -- Este não precisa utilziar, por padrão, onde não tem ação, aplica-se esse automaticamente
    deriving (Show, Eq, Generic, ToJSON)

exampleEpisode :: Episode
exampleEpisode = Episode
    { ePes = [EPeLabel "pe_felipe", EPeLabel "pe_fatima"]
    , eDialoguePeList = exampleDialogues
    }
    where
        exampleDialogues :: [EDialoguePe]
        exampleDialogues =
            [ EDialoguePe
                { dPe = EPeLabel "pe_felipe"
                , dContents =
                    [ RPlainText "Olá Fatima"
                    , RCommand (CGesture GHi (EPeLabel "pe_felipe"))
                    , RCommand (CGesture GHi (EPeLabel "pe_fatima"))
                    , RCommand (CPause 500)
                    , RPlainText "Tudo bem por aí?"
                    ]
                }
            , EDialoguePe
                { dPe = EPeLabel "pe_fatima"
                , dContents =
                    [ RPlainText "Olá Felipe! Tudo ótimo!"
                    , RCommand (CGesture GHi (EPeLabel "pe_fatima")) ]
                }
            ]