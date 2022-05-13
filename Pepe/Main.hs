module Main where

import           System.Environment (getArgs)
import           System.Exit        (exitFailure)

import           Pepe.Interpreter   (interpretFile)


main :: IO ()
main = do
    args@(filepath : _) <- getArgs
    case length args of
        1 -> do
            interpretFile filepath
        _ -> do
            putStrLn "Invalid number of arguments! Run with arguments: (filepath)."
            exitFailure

-- ghc --make Pepe/Main.hs -o interpreter
