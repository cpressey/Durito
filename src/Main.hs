module Main where

import System.Environment
import System.Exit
import System.IO

import Language.Diodorus.Model
import qualified Language.Diodorus.Env as Env
import qualified Language.Diodorus.Parser as Parser
import qualified Language.Diodorus.Eval as Eval
import qualified Language.Diodorus.Residuator as Residuator


main = do
    args <- getArgs
    case args of
        ["parse", fileName] -> do
            program <- loadSource fileName
            putStrLn $ show program
        ["eval", fileName] -> do
            program <- loadSource fileName
            let globals = Eval.makeInitialEnv program
            case Env.fetch "main" globals of
                Nothing ->
                    abortWith "No main function defined"
                Just (Fun [] main _) -> do
                    let result = Eval.evalExpr globals (Env.empty) main
                    putStrLn $ show result
        ["residuate", fileName] -> do
            program <- loadSource fileName
            let globals = Residuator.makeInitialEnv program
            case Env.fetch "main" globals of
                Nothing ->
                    abortWith "No main function defined"
                Just (Just (Fun [] main _)) -> do
                    let result = Residuator.residuateExpr globals (Env.empty) main
                    putStrLn $ show result
        _ -> do
            abortWith "Usage: diodorus (parse|eval|residuate) <input-filename>"

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
