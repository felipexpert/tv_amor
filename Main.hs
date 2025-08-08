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

import qualified Model.Test2 as Test2

import qualified Model.Test3 as Test3

main_ :: IO ()
main_ = do 
    setLocaleEncoding utf8
    hSetEncoding stdout utf8
    hSetEncoding stdin utf8
    hSetEncoding stderr utf8

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

main :: IO ()
main = do 
    setLocaleEncoding utf8
    -- hSetEncoding stdout utf8
    -- hSetEncoding stdin utf8
    -- hSetEncoding stderr utf8
    
    Test3.testIO

