{-# LANGUAGE OverloadedStrings #-}

-- MyModule.hs
module Model.MyModule where

import qualified Data.Text as T
import Data.Text (Text)

mensagem :: Text
mensagem = "Mensagem vindo de outro módulo!"
