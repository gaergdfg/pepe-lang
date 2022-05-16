module Interpreter (interpretFile) where

import           System.Exit (ExitCode (ExitFailure), exitFailure, exitSuccess,
                              exitWith)
import           System.IO   (hPrint, stderr)

import           Evaluator   (Value (VInt), evaluate)
import           Pepe.Par    (myLexer, pProgram)
import           Typechecker (typecheck)


interpretFile :: FilePath -> IO ()
interpretFile filepath = do
    input <- readFile filepath
    let tokens = myLexer input
    case pProgram tokens of
        Left err    -> exitWithError err
        Right program -> do
            case typecheck program of
              Left err -> exitWithError err
              Right _  -> do
                result <- evaluate program
                case result of
                    Left err -> exitWithError err
                    Right (VInt value) ->
                        if value == 0
                        then exitSuccess
                        else exitWith (ExitFailure (fromIntegral value))
                    Right _ -> exitWithError "Unexpected return type"


exitWithError :: Show a => a -> IO ()
exitWithError err = do
    hPrint stderr err
    exitFailure
