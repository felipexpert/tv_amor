{-# LANGUAGE OverloadedStrings #-}

module Model.Test where

import System.Process
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import Control.Monad (void)
import System.IO (hClose)

-- Representa o JSON de entrada
data Comando = Comando
    { acao :: Text
    , a    :: Int
    , b    :: Int
    } deriving (Show)

instance ToJSON Comando where
    toJSON (Comando acao a b) =
        object ["acao" .= acao, "a" .= a, "b" .= b]

-- Representa o JSON de resposta
newtype Resposta = Resposta { resultado :: Int }
    deriving (Show)

instance FromJSON Resposta where
    parseJSON = withObject "Resposta" $ \obj ->
        Resposta <$> obj .: "resultado"

-- Função principal que envia JSON, lê resposta e imprime
executarComunicacao :: IO ()
executarComunicacao = do
    let comando = Comando "somar" 3 5
        inputJson = encode comando

    (Just hin, Just hout, _, _) <- createProcess (proc "python" ["ModelGL/guido_responde.py"])
        { std_in = CreatePipe, std_out = CreatePipe }

    BL.hPutStr hin inputJson
    hClose hin

    resposta <- BL.hGetContents hout
    putStrLn "Resposta do Python:"
    BL.putStrLn resposta
