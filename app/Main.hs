{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Prelude hiding       (read, repeat, lookup)
import Data.List            (find)
import Data.Map hiding      (drop, take)
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

validate :: String -> Config -> Maybe String
validate input config = do
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

    -- Validate input
    else if (length input) == 0
        then Just "input is empty"
    else if not (all (\c -> [c] `elem` (alphabet config)) input)
        then Just "input should be in alphabet"
    else if ((head (blank config)) `elem` input)
        then Just "input should not contain blank"

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

padLeft :: Int -> String -> String -> String
padLeft n padding s
    | len < n = repeat (n - len) padding ++ s
    | otherwise = s
    where len = length s

padRight :: Int -> String -> String -> String
padRight n padding s
    | len < n = s ++ repeat (n - len) padding
    | otherwise = s
    where len = length s

limitLeft :: Int -> String -> String
limitLeft n s
    | len > n = drop (len-n) s
    | otherwise = s
    where len = length s

limitRight :: Int -> String -> String
limitRight n s
    | len > n = take n s
    | otherwise = s
    where len = length s

data Tape = Tape {
    left :: String,
    current :: String,
    right :: String
} deriving (Show)

newTape :: String -> Tape
newTape input = Tape "" (take 1 input) (drop 1 input)

rDrop :: Int -> String -> String
rDrop n xs = reverse (drop n (reverse xs))

rTake :: Int -> String -> String
rTake n xs = reverse (take n (reverse xs))

moveTape :: String -> String -> Tape -> Tape
moveTape "RIGHT" blank tape = Tape ((left tape) ++ (current tape)) (padRight 1 blank (take 1 (right tape))) (drop 1 (right tape))
moveTape "LEFT" blank tape = Tape (rDrop 1 (left tape)) (padLeft 1 blank (rTake 1 (left tape))) ((current tape) ++ (right tape))

tapeToString :: Tape -> Int -> String -> String
tapeToString tape n blank = do
    "["
        ++ "\x1b[0;90m" ++ (limitLeft n (padLeft n blank (left tape)))
        ++ "\x1b[1;93m" ++ (current tape)
        ++ "\x1b[0;90m" ++ (limitRight n (padRight n blank (right tape)))
        ++ "\x1b[0m]"

repeat :: Int -> String -> String
repeat 1 s = s
repeat i s = repeat (i-1) s ++ s

putError :: String -> IO ()
putError msg = do
    putStrLn ("\x1b[1;91merror\x1b[0m: " ++ msg)

execute :: Config -> String -> Tape -> IO ()
execute config state tape = do
    putStrLn ((tapeToString tape 15 (blank config)) ++ " \x1b[1;91m" ++ state ++ "\x1b[0m")

    -- check if in final state
    if state `elem` (finals config) then do
        putStrLn "+++ final state reached +++"
    else do
        case lookup state (transitions config) of
            Just transitions -> do
                case find (\t -> (read t) == (current tape)) transitions of
                    Just transition -> do
                        putStrLn (" \x1b[1;94m↓\x1b[0m " ++ (show transition))
                        let writtenTape = (Tape (left tape) (write transition) (right tape))
                        execute config (to_state transition) (moveTape (action transition) (blank config) writtenTape)
                    Nothing -> do
                        putError "blocked (no transition found)"
            Nothing -> do
                putError "blocked (no transition found)"

main :: IO ()
main = do
    args <- getArgs

    if (length args /= 2) || (any (\arg -> arg == "-h" || arg == "--help") args) then do
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
                case validate input json of
                    Just err -> putError err
                    Nothing  -> execute json (initial json) (newTape input)
            Nothing   -> putError "parsing json config"