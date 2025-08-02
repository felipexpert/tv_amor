{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- MyModule.hs
module Model.Episode where

import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics (Generic)



-- Representa o personagem que está falando
newtype CharLabel = CharLabel Text
    deriving (Show, Eq, Ord, Generic)

-- Representa os comandos embutidos no texto
data InlineCommand
    = Wave CharLabel
    | Pause Double
    deriving (Show, Eq)

-- Um trecho de texto com comandos embutidos
data RichText
    = PlainText Text
    | Command InlineCommand
    deriving (Show, Eq)

-- Um bloco de fala, associado a um personagem
data DialogueBlock = DialogueBlock
    { speaker :: CharLabel
    , contents :: [RichText]
    } deriving (Show, Eq)

-- O episódio completo (lista de blocos)
type Episode = [DialogueBlock]

exampleEpisode :: Episode
exampleEpisode =
    [ DialogueBlock
        { speaker = CharLabel "char_felipe"
        , contents =
            [ PlainText "Olá Gisele"
            , Command (Wave (CharLabel "char_felipe"))
            , Command (Wave (CharLabel "char_gisele"))
            , Command (Pause 0.5)
            , PlainText "Tudo bem por aí?"
            ]
        }
    , DialogueBlock
        { speaker = CharLabel "char_gisele"
        , contents =
            [ PlainText "Olá Felipe! Tudo ótimo!" ]
        }
    ]