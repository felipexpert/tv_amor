{-# LANGUAGE OverloadedStrings #-}

module TV where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Text (Text)

import System.IO
import GHC.IO.Encoding

import Model.MyModule (mensagem)

main :: IO ()
main = do 
    setLocaleEncoding utf8
    TIO.putStrLn mensagem
