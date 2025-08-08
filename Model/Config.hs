{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Model.Config where

import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

import GHC.Generics (Generic) 
import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy as B



data Config = Config
    { workingDir :: FilePath

    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- Lê o arquivo "config.json" na raiz do projeto e retorna o Config
loadConfigIO :: IO Config
loadConfigIO = do
    cwd <- getCurrentDirectory
    let path = cwd </> "config.json"
    content <- B.readFile path
    case decode content of
        Just cfg -> return cfg
        Nothing -> error "Falha ao ler ou decodificar o arquivo config.json"