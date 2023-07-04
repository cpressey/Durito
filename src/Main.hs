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
                Just (Residuator.Known value) -> do
                    case (residuateGlobal globals "main" value) of
                        Just value' -> putStrLn $ Pretty.renderValue value'
                        Nothing ->
                            abortWith "main function could not be residuated"
        ["residuate", fileName] -> do
            program <- loadSource fileName
            let globals = Residuator.makeInitialEnv program
            let glah = Env.foldrWithKey (\name (Residuator.Known value) acc -> acc ++ [residuateGlobal globals name value]) [] globals
            putStrLn $ show glah
        _ -> do
            abortWith "Usage: durito (parse|eval|residuate|residuate-main) <input-filename>"

residuateGlobal globals name fun@(Fun formals body denv) =
    let
        body' = Residuator.residuateFunDefn globals (Env.empty) fun
    in
        Just $ Fun formals body' denv
residuateGlobal globals name other =
    Nothing

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
