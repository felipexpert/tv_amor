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
import Model.AudiosInfo(AudioInfo)

import Model.EpisodePersona (EPeNumber(..))
import Model.Episode(CGesture(..))

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

{-
-- ***** START - tipo intermediário entre EDialoguePe e AudioInfo e duration - START *****
-- utilizado para criar os TPeAction do AniAutoTask
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
    | CPause Double
    deriving (Show, Eq)

data CGesture
    = GWave
    | GThink1 -- detalhes como duração, podem variar, por isso tem EGThing1 e EGThink2, etc
    | GThink2
    deriving (Show, Eq) 
-- ***** END   - tipo intermediário entre EDialoguePe e AudioInfo e duration - END   *****
-}

{-
-- aqui concatenamos os trechos de fala de cada personagem, e somamos o tempo total
dialoguesToActions :: [EDialoguePeAI] -> ([TPeAction], Int)
dialoguesToActions = undefined

-- cada trecho em que um personagem fala, gera um conjunto de ações, e o total de tempo
dialogueToActions :: EDialoguePeAI -> ([TPeAction], Int)
dialogueToActions dialogues = map convertToTPeAction edpaList
    where
        -- Função principal
        calcB :: [EDialoguePeAI] -> ([TPeAction], Int)
        calcB dialogues = go dialogues 0 []
            where
                go [] total acc = (reverse acc, total)
                go (d:ds) currentTime acc = go ds nextTime (action : acc)
                    where
                        action = B x currentTime
                        nextTime = currentTime + getDialogDuration d
                        getDialogDuration :: EDialoguePeAI -> Int
                        getDialogDuration = undefined
-}