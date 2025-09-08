{-# LANGUAGE OverloadedStrings #-}

module Model.JSONUtil where

import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (ToJSON, FromJSON, encode)


savePrettyIO :: ToJSON a => FilePath -> a -> IO ()
savePrettyIO path value = BL.writeFile path (Pretty.encodePretty value)
