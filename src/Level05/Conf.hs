{-# LANGUAGE OverloadedStrings #-}
module Level05.Conf
    ( Conf (..)
    , firstAppConfig
    ) where

data Conf = Conf
  { dbFilePath :: FilePath
  , getPort :: Int
  }

firstAppConfig :: Conf
firstAppConfig = Conf "app_db.db" 3000
