{-# LANGUAGE OverloadedStrings #-}

module Model.TextUtil where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Text (Text)

putShowable_old :: Show a => a -> IO ()
putShowable_old s = TIO.putStrLn (T.pack (show s))

putShowable :: Show a => a -> IO ()
putShowable x = TIO.putStrLn $ T.pack $ formatShow $ show x
  where
    formatShow :: String -> String
    formatShow = read . show -- This converts escaped characters back to their Unicode form