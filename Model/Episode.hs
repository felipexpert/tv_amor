{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-tabs #-}

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

-- A "FREQUÊNCIA" é o quanto um gesto normalmente aparece, quanto maior o valor da "FREQUÊNCIA",
-- mais o gesto deve ser aplicado em relação aos outros gestos
data CGesture
    = GThinkShort       -- FREQUÊNCIA: 10.0		- Coloca a mão no queixo, reflexivo
    | GThinkLong        -- FREQUÊNCIA: 9.5		- Coloca a mão no queixo, reflexivo, por mais tempo
	  | GStandShort		  	-- FREQUÊNCIA: 9.0		- Fica com as mãos juntas, depois volta as mãos
    | GStandLong	  		-- FREQUÊNCIA: 8.5		- Fica com as mãos juntas por mais tempo, depois volta as mãos, por mais tempo
	  | GShakeLegShort		-- FREQUÊNCIA: 6.0		- Balança um pouco a perna
    | GWorryShort		  	-- FREQUÊNCIA: 1.0		- Demonstra um pouco de preocupação

    -- Não indicados para automação:
    | GHi					      -- Acena
	  | GTalkShort		  	-- Movimenta os braços, expressando ideias
    | GTalkLong			  	-- Movimenta os braços, expressando ideias, por mais tempo
    | GShakeLegLong			-- Balança um pouco a perna, por mais tempo
    | GExcited -- fica animado!
    | GDance -- Dança de alegria (um pouco exagerado, não utilize tanto esse)
    | GDefault
    | GWorryLong -- demonstra um pouco de preocupação, por mais tempo
    deriving (Show, Eq, Generic, ToJSON)

-- Função que insere uma pequena pausa (500ms) caso a primeira ação do primeiro diálogo
-- seja uma fala (RPlainText)
addPauseIfNeeded :: Episode -> Episode
addPauseIfNeeded ep = ep { eDialoguePeList = newDialogueList }
    where 
        newDialogueList :: [EDialoguePe]
        newDialogueList = case eDialoguePeList ep of
            [] -> []
            (firstBlock:restBlocks) -> 
                let newFirstBlock = addPauseToFirstDialog firstBlock
                in newFirstBlock : restBlocks

        addPauseToFirstDialog :: EDialoguePe -> EDialoguePe
        addPauseToFirstDialog block = block { dContents = newContents }
            where 
                newContents :: [DRichText]
                newContents = case dContents block of
                    [] -> []
                    (firstContent:restContents) -> case firstContent of
                        RPlainText _ -> RCommand (CPause 500) : firstContent : restContents
                        _            -> firstContent : restContents

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