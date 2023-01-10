{-# LANGUAGE DeriveGeneric #-}

module Lib ( loadConfig ) where

import Data.Map
import Data.Aeson           (FromJSON, decode)
import GHC.Generics         (Generic)
import Data.ByteString.Lazy (readFile)

data Transition = Transition {
    read :: String,
    to_state :: String,
    write :: String,
    action :: String
} deriving (Show, Generic)
instance FromJSON Transition

data Config = Config {
    name :: String,
    alphabet :: [String],
    blank :: String,
    states :: [String],
    initial :: String,
    finals :: [String],
    transitions :: Map String [Transition]
} deriving (Show, Generic)
instance FromJSON Config

loadConfig :: String -> IO (Maybe Config)
loadConfig jsonfile = do
    config <- Data.ByteString.Lazy.readFile jsonfile
    return (decode config)