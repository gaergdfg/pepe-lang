module Pepe.Interpreter where

import           System.Exit (exitSuccess)

import           Pepe.Par    (myLexer, pProgram)


interpretFile :: FilePath -> IO ()
interpretFile filepath = do
    input <- readFile filepath
    let tokens = myLexer input
    let program = pProgram tokens
    -- type check
    -- evaluate
    exitSuccess
