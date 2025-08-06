{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Text (Text)
import qualified Model.TextUtil as TU

import System.IO
import GHC.IO.Encoding

import Model.MyModule (mensagem)

import Model.Test (fazerComunicacao)

import Model.Episode (exampleEpisode)
import Model.EpisodeSetup (exampleEpisodeSetup) 
import Model.EpisodeComplete (EpisodeComplete(..))

main :: IO ()
main = do 
    setLocaleEncoding utf8

    TIO.putStrLn mensagem
    TIO.putStrLn "Fazendo comunicação com GuidoLang..."
    fazerComunicacao
    TIO.putStrLn "Comunicação concluída"
    TIO.putStrLn ""
    let ep = exampleEpisode
    TU.putShowable ep
    TIO.putStrLn ""
    let setup = exampleEpisodeSetup
    TU.putShowable setup