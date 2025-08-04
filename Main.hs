{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Text (Text)
import qualified Model.TextUtil as TU

import System.IO
import GHC.IO.Encoding

import Model.MyModule (mensagem)

import Model.Test (executarComunicacao)

import Model.Constants (tvAmorBaseDir)

import Model.Episode (exampleEpisode)
import Model.EpisodeSetup (exampleEpisodeSetup)

main :: IO ()
main = do 
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    setLocaleEncoding utf8

    TIO.putStrLn mensagem
    TIO.putStrLn "Executando comunicação com GuidoLang..."
    executarComunicacao
    TIO.putStrLn "Comunicação concluída"
    TIO.putStrLn ""
    let ep = exampleEpisode
    TU.putShowable ep
    TIO.putStrLn ""
    let setup = exampleEpisodeSetup
    TU.putShowable setup