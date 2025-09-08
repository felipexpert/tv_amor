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

import qualified Model.EpisodeSetup as ES
import qualified Model.Config as C
import qualified Model.JSONUtil as JU

import qualified Model.Test2 as Test2

import qualified Model.WorkingEpisode as WE

import System.FilePath ((</>))

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

main_2 :: IO ()
main_2 = do 
  setLocaleEncoding utf8
  let e = ES.SBackground
        { ES.bImagePath = "guga-doces-e-fraldas-17.jpg"
        , ES.bWidth = 1080
        , ES.bHeight = 1920
        , ES.bSpritePositions = ES.SPositionsFor2
            { ES.pFor2Sprite1 = ES.PSprite (-46) (-13)
            , ES.pFor2Sprite2 = ES.PSprite 46 (-13)
            }
        }
  config <- C.loadConfigIO
  let path = C.backgroundDir config </> "test-bg.json"
  JU.savePrettyIO path e
  TU.putShowable e

main :: IO ()
main = do 
  setLocaleEncoding utf8
  -- hSetEncoding stdout utf8
  -- hSetEncoding stdin utf8
  -- hSetEncoding stderr utf8
  
  WE.buildEpisodeIO

