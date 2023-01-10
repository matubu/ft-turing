module Main (main) where

import Lib                (loadConfig)
import System.Environment (getArgs)

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

        config <- loadConfig (jsonfile)
        case config of
            Just json -> print json
            Nothing   -> putStrLn "error: invalid config"
