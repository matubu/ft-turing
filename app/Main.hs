{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Prelude hiding       (read, lookup)
import Data.Map
import Data.Aeson           (FromJSON, decode)
import GHC.Generics         (Generic)
import Data.ByteString.Lazy (readFile)
import System.Environment   (getArgs)

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
    json <- Data.ByteString.Lazy.readFile jsonfile
    return (decode json)

validateConfig :: Config -> Maybe String
validateConfig config = do
    -- Validate alphabet
    if (length (alphabet config)) == 0
        then Just "alphabet is empty"
    else if (any (\c -> length c /= 1) (alphabet config))
        then Just "alphabet should be only one char"
    
    -- Validate blank
    else if (length (blank config)) /= 1
        then Just "blank should be only one char"
    else if not ((blank config) `elem` (alphabet config))
        then Just "blank should be in alphabet"
    
    -- Validate states
    else if (length (states config)) == 0
        then Just "states is empty"
    else if any (\s -> length s == 0) (states config)
        then Just "states should be at least one char"
    
    -- Validate initial
    else if not ((initial config) `elem` (states config))
        then Just "initial should be in states"

    -- Validate finals
    else if (length (finals config)) == 0
        then Just "finals is empty"
    else if not (all (\f -> f `elem` (states config)) (finals config))
        then Just "finals should be in states"
    
    -- Validate transitions
    else if (length (transitions config)) == 0
        then Just "transitions is empty"
    else if not (all (\s -> s `elem` (states config)) (keys (transitions config)))
        then Just "transitions names should be in states"
    
    -- Validate transitions.read
    else if not (all (\t -> (read t) `elem` (alphabet config)) (concat (elems (transitions config))))
        then Just "transitions.read should be in alphabet"

    -- Validate transitions.write
    else if not (all (\t -> (write t) `elem` (alphabet config)) (concat (elems (transitions config))))
        then Just "transitions.write should be in alphabet"
    
    -- Validate transitions.to_state
    else if not (all (\t -> (to_state t) `elem` (states config)) (concat (elems (transitions config))))
        then Just "transitions.to_state should be in states"
    
    -- Validate transitions.action
    else if not (all (\t -> (action t) `elem` ["LEFT", "RIGHT"]) (concat (elems (transitions config))))
        then Just "transitions.action should be LEFT or RIGHT"
    
    else Nothing

-- arguments = (config state tape)
execute :: Config -> String -> String -> IO ()
execute config state input = do
    putStrLn "state: "

    -- if state `elem` (finals config) then return ()
    -- return if in final state

    case lookup state (transitions config) of
        Just transitions -> do
            -- case find (\t -> (read t) == (take 1 input)) transitions of
            --     Just transition -> do
            --         -- putStrLn "transition: "
            --         -- putStrLn "read: "
            --         -- putStrLn "to_state: "
            --         -- putStrLn "write: "
            --         -- putStrLn "action: "
            --         -- execute config (to_state transition) (drop 1 input)
            --     Nothing -> do
            --         putStrLn "error: no transition found"
            --         putStrLn "transitions: "
            print transitions
            putStrLn "ok"
        Nothing -> do
            putStrLn "error: invalid state"

-- TODO check for help flag
-- TODO infinite tape with index
main :: IO ()
main = do
    args <- getArgs

    if length args /= 2 then do
        putStrLn "usage: ft_turing [-h] jsonfile input"
        putStrLn ""
        putStrLn "positional arguments:"
        putStrLn "  jsonfile json description of the machine"
        putStrLn "  input input of the machine"
        putStrLn ""
        putStrLn "optional arguments:"
        putStrLn "  -h, --help show this help message and exit"
    else do
        let (jsonfile, input) = (args !! 0, args !! 1)
    
        config <- loadConfig jsonfile
        case config of
            Just json -> do
                case validateConfig json of
                    Just err -> putStrLn ("error: " ++ err)
                    Nothing  -> execute json (initial json) input
            Nothing   -> putStrLn "error: parsing json config"
