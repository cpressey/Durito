module Main where

import System.Environment
import System.Exit
import System.IO

import Language.Diodorus.Model
import qualified Language.Diodorus.Env as Env
import qualified Language.Diodorus.Parser as Parser
import qualified Language.Diodorus.Eval as Eval
import qualified Language.Diodorus.Residuator as Residuator
import qualified Language.Diodorus.Pretty as Pretty


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
                Just (Fun formals main _) -> do
                    -- TODO: read arguments from command line
                    -- TODO: create env from those arguments
                    let result = Eval.evalExpr globals (Env.empty) main
                    putStrLn $ Pretty.renderValue result
        ["residuate-main", fileName] -> do
            program <- loadSource fileName
            let globals = Residuator.makeInitialEnv program
            case Env.fetch "main" globals of
                Nothing ->
                    abortWith "No main function defined"
                Just (Residuator.Known fun@(Fun formals main _)) -> do
                    let result = Residuator.residuateFunDefn globals (Env.empty) fun
                    putStrLn $ Pretty.renderExpr result
        _ -> do
            abortWith "Usage: diodorus (parse|eval|residuate-main) <input-filename>"

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
