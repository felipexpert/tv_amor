{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- MyModule.hs
module Model.AniAutoTask where

import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics (Generic) 
import Model.EpisodeComplete
import Model.AudiosInfo(AudioInfo, aiDuration, aiFilePath)

import Model.EpisodePersona (EPeNumber(..))
import Model.Episode(CGesture(..))
import Model.EpisodePersona(EPeLabel(..))

data AniAutoTask = AniAutoTask
    { aatActions :: [TPeAction]
    , aatTotalDuration :: Int -- Duração total do episódio em milissegundos
    } 
    deriving (Show, Eq, Generic)

data TPeAction = TPeAction
    { tpaNumber :: EPeNumber
    , tpaAction :: AAction
    }
    deriving (Show, Eq, Generic)

data AAction
    = ASpeech
        { asAudioWav :: FilePath -- Caminho do arquivo de áudio
        , asStartTime :: Int -- Tempo de início da fala
        }
    | AGesture
        { agGesture :: CGesture 
        , agStartTime :: Int -- Tempo de início do gesto 
        }
    deriving (Show, Eq, Generic)

-- você pode criar gestos compostos no CA4 e vincular aqui!


-- ***** START - tipo intermediário entre EDialoguePe e AudioInfo e duration - START *****
-- utilizado para criar os TPeAction do AniAutoTask
data TDialoguePe = EDialoguePe
    { dPe :: EPeLabel
    , dPeNumber :: EPeNumber
    , dContents :: [DRichText]
    } deriving (Show, Eq)

-- Um trecho de texto com comandos embutidos
data DRichText
    = RPlainText RSpeech
    | RCommand RCCommand
    deriving (Show, Eq)

dRichTextDuration :: DRichText -> Int
dRichTextDuration (RPlainText rs) = aiDuration (rsAudioInfo rs)
dRichTextDuration (RCommand _) = 0 -- Comandos não têm duração de áudio
dRichTextDuration (RCommand (CPause duration)) = duration

data RSpeech = RSpeech
    { rsText :: Text -- Texto a ser falado
    , rsAudioInfo :: AudioInfo -- Informações do áudio associado
    }
    deriving (Show, Eq)

-- Representa os comandos embutidos no texto
data RCCommand 
    -- ERTCGesture tem quem está fazendo o gesto, porque durante a fala de um, o outro pode fazer um gesto
    = CGesture
        { cGesture :: CGesture
        , cPe :: EPeLabel
        , cPeNumber :: EPeNumber
        }
    | CPause Int
    deriving (Show, Eq)
-- ***** END   - tipo intermediário entre EDialoguePe e AudioInfo e duration - END   *****


-- aqui concatenamos os trechos de fala de cada personagem, e somamos o tempo total
dialoguesToActions :: [TDialoguePe] -> ([TPeAction], Int)
dialoguesToActions dialogues = (actions, totalTime)
    where
        actionsAndTimes :: [([TPeAction], Int)]
        actionsAndTimes = map dialogueToActions dialogues
        actions :: [TPeAction]
        actions = concatMap fst actionsAndTimes
        totalTime :: Int
        totalTime = sum $ map snd actionsAndTimes


-- cada trecho em que um personagem fala, gera um conjunto de ações, e o total de tempo
dialogueToActions :: TDialoguePe -> ([TPeAction], Int)
dialogueToActions dialogue = generateActions' (dContents dialogue)
    where
        peNumber :: EPeNumber
        peNumber = dPeNumber dialogue
        generateActions :: [DRichText] -> ([Maybe TPeAction], Int)
        generateActions ts = go ts 0 []
            where
                go [] total acc = (reverse acc, total)
                go (t:ts) currentTime acc = go ts nextTime (actionOpt : acc)
                    where
                        actionOpt = case t of
                            RPlainText speech -> Just $ speechToAction speech
                            -- Atenção! peNumberG é o número do personagem que faz o gesto, pode ser diferente do peNumber
                            RCommand (CGesture gesture _ peNumberG) -> Just $ gestureToAction gesture peNumberG
                            RCommand (CPause duration) -> Nothing
                        nextTime = currentTime + dRichTextDuration t
                        speechToAction :: RSpeech -> TPeAction
                        speechToAction speech = TPeAction peNumber (ASpeech (aiFilePath (rsAudioInfo speech)) currentTime)
                        gestureToAction :: CGesture -> EPeNumber -> TPeAction
                        gestureToAction gesture peNumberG = TPeAction peNumber (AGesture gesture currentTime)
        generateActions' :: [DRichText] -> ([TPeAction], Int)
        generateActions' ts = (actions, totalTime)
            where 
                (actionOpts, totalTime) = generateActions ts
                actions = [a | Just a <- actionOpts] -- filtra as ações válidas
