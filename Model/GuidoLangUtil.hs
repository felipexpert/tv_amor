{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

-- MyModule.hs
module Model.GuidoLangUtil where

import GHC.Generics (Generic)
import System.Process
import System.IO (hClose)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (ToJSON, FromJSON, encode, eitherDecode)
import Control.Exception (throwIO)

-- Função principal genérica
glCall :: (ToJSON a, FromJSON b) => GLScript -> a -> IO b
glCall script input = do
    let scriptPath = getGLScriptPath script
        inputJson = encode input

    -- Chama o processo GL
    (Just hin, Just hout, _, _) <- createProcess (proc "python" [scriptPath])
        { std_in = CreatePipe, std_out = CreatePipe }

    -- Envia o JSON para o GL
    BL.hPutStr hin inputJson
    hClose hin

    -- Lê a resposta
    respostaJson <- BL.hGetContents hout

    -- Decodifica a resposta e retorna
    case eitherDecode respostaJson of
        Right result -> return result
        Left err -> do
            putStrLn "Erro ao decodificar resposta JSON do GuidoLang:"
            putStrLn err
            putStrLn "Resposta recebida:"
            putStrLn (show respostaJson)
            throwIO (userError "Falha ao decodificar JSON de resposta do GuidoLang")

data GLScript
    = GLAudiosInfo
    -- vai ter outros
    deriving (Show, Eq, Generic)

getGLScriptPath :: GLScript -> String
getGLScriptPath GLAudiosInfo = "ModelGL/audios_info.py"