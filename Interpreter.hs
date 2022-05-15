module Interpreter (interpretFile) where

import           System.Exit (exitFailure, exitSuccess)
import           System.IO   (hPrint, stderr)

import           Pepe.ErrM   (Err (Bad, Ok))
import           Pepe.Par    (myLexer, pProgram)
import           Typechecker (typecheck)


interpretFile :: FilePath -> IO ()
interpretFile filepath = do
    input <- readFile filepath
    let tokens = myLexer input
    case pProgram tokens of
        Bad err    -> exitWithError err
        Ok program -> do
            case typecheck program of
              Left err -> exitWithError err
              Right _  -> do
                -- evaluate
                exitSuccess


exitWithError :: Show a => a -> IO ()
exitWithError err = do
    hPrint stderr err
    exitFailure