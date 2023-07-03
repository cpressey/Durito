module Main where

import System.Environment
import System.Exit
import System.IO

import Language.Durito.Model
import qualified Language.Durito.Env as Env
import qualified Language.Durito.Parser as Parser
import qualified Language.Durito.Eval as Eval
import qualified Language.Durito.Residuator as Residuator
import qualified Language.Durito.Pretty as Pretty


main = do
    args <- getArgs
    case args of
        ["parse", fileName] -> do
            program <- loadSource fileName
            putStrLn $ show program
        ("eval":(fileName: progArgs)) -> do
            program <- loadSource fileName
            let actuals = map (Parser.parseLiteral) progArgs
            let result = Eval.evalProgram program actuals
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
            abortWith "Usage: Durito (parse|eval|residuate-main) <input-filename>"

loadSource fileName = do
    handle <- openFile fileName ReadMode
    -- hSetEncoding handle utf8
    text <- hGetContents handle
    case Parser.parseDurito text of
        Right g -> do
            return g
        Left error ->
            abortWith $ show error

abortWith msg = do
    hPutStrLn stderr msg
    exitWith $ ExitFailure 1
