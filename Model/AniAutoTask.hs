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

data AniAutoTask = AniAutoTask
    { aatActions :: [TPeAction]
    -- depois pode ter mais campos
    } 
    deriving (Show, Eq, Generic)

data TPeAction = TPeAction
    { tpaNumber :: TPeNumber
    , tpaAction :: AAction
    }
    deriving (Show, Eq, Generic)

data AAction
    = ASpeech
        { asAudioWav :: FilePath -- Caminho do arquivo de áudio
        , asStartTime :: Int -- Tempo de início da fala
        }
    | AGesture
        { agGesture :: AGGesture 
        , agStartTime :: Int -- Tempo de início do gesto 
        }
    deriving (Show, Eq, Generic)

-- você pode criar gestos compostos no CA4 e vincular aqui!
data AGGesture
    = AGWave1
    | AGThink1
    deriving (Show, Eq, Generic)

-- para identificar o sprite da pessoa na TPeAction
data TPeNumber 
    = PNumber1
    | PNumber2
    deriving (Show, Eq, Generic)