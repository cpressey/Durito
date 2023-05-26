module Main where

import System.Environment
import System.Exit
import System.IO

import qualified Language.Diodorus.Env as Env
import qualified Language.Diodorus.Eval as Eval
import qualified Language.Diodorus.Parser as Parser


main = do
    args <- getArgs
    case args of
        ["parse", fileName] -> do
            program <- loadSource fileName
            putStrLn $ show program
        ["eval", fileName] -> do
            program <- loadSource fileName
            let env = Env.makeEnv program
            case Env.fetch "main" env of
                Nothing ->
                    abortWith "No main function defined"
                Just main -> do
                    let result = Eval.evalExpr env main
                    putStrLn $ show result
        _ -> do
            abortWith "Usage: diodorus (parse) <input-filename>"

loadSource fileName = do
    handle <- openFile fileName ReadMode
    -- hSetEncoding handle utf8
    text <- hGetContents handle
    case Parser.parseDiodorus text of
        Right g -> do
            return g
        Left error ->
            abortWith $ show error

abortWith msg = do
    hPutStrLn stderr msg
    exitWith $ ExitFailure 1
