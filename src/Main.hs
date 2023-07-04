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
            putStr $ Pretty.renderProgram program
        ("eval":(fileName: progArgs)) -> do
            program <- loadSource fileName
            let actuals = map (Parser.parseLiteral) progArgs
            let result = Eval.evalProgram program actuals
            putStrLn $ Pretty.renderValue result
        ["residuate", fileName] -> do
            program <- loadSource fileName
            putStr $ Pretty.renderProgram $ Residuator.residuateProgram program
        _ -> do
            abortWith "Usage: durito (parse|eval|residuate) <input-filename>"

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
