{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Text (Text)

import System.IO
import GHC.IO.Encoding

import Model.MyModule (mensagem)

import Model.Test (executarComunicacao)

main :: IO ()
main = do 
    setLocaleEncoding utf8
    TIO.putStrLn mensagem
    TIO.putStrLn "Executando comunicação com GuidoLang..."
    executarComunicacao
    TIO.putStrLn "Comunicação concluída"
