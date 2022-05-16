{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module Evaluator where

import           Control.Monad.Except (ExceptT, MonadError (throwError),
                                       MonadIO (liftIO), runExceptT)
import           Control.Monad.Reader ()
import           Control.Monad.State  (MonadState (get, put), StateT,
                                       evalStateT, gets, modify)
import           Data.Maybe           (fromJust)

import           Pepe.Abs             (AddOp' (..), Arg, Arg' (..),
                                       BNFC'Position, Block, Block' (..), Expr,
                                       Expr' (..), Ident (..), MulOp' (..),
                                       Program, Program' (..), RelOp' (..),
                                       Stmt, Stmt' (..), TopDef, TopDef' (..),
                                       Type, Type' (..), showPos)

import qualified Data.Map             as M


-- Types ---------------------------------------------------------------------------------
data Value
    = VBool Bool
    | VInt Integer
    | VString String
    | VVoid
    | VFunc [Arg] Block Env'
    | None
    deriving (Eq)

instance Show Value where

    show (VBool val)   = show val

    show (VInt val)    = show val

    show (VString val) = val

    show _             = ""


-- Exceptions ----------------------------------------------------------------------------
type RuntimeException = RuntimeException' BNFC'Position
data RuntimeException' a
    = UnimplementedException a String
    | DivisionByZeroException a
    | ReturnNotCalledException a


instance Show RuntimeException where

    show (UnimplementedException pos msg) = "Error - unimplemented functionality! " ++ msg

    show (DivisionByZeroException pos) = "Error - division by zero at: " ++ showPos pos

    show (ReturnNotCalledException pos) =
        "Error - return was not called at the end of block starting at: " ++ showPos pos


-- Environment ---------------------------------------------------------------------------
type Env' = M.Map Ident Value

data Env = Env
    { env_   :: Env'
    , retVal :: Value
    }

initEnv :: Env
initEnv = Env
    { env_   = M.empty
    , retVal = None
    }

getEnv :: Env -> Env'
getEnv = env_

getRetVal :: Env -> Value
getRetVal = retVal

getVarValue :: Ident -> Env -> Value
getVarValue name Env{..} = fromJust $ M.lookup name env_

updateEnv :: Ident -> Value -> Env -> Env
updateEnv name val Env{..} = Env
    { env_   = M.insert name val env_
    , retVal = retVal
    }

updateRetVal :: Value -> Env -> Env
updateRetVal val Env{..} = Env
    { env_   = env_
    , retVal = val
    }

builtinFunction :: [Ident]
builtinFunction =
    [ Ident "printBool"
    , Ident "printInt"
    , Ident "printString"
    ]


-- Monads --------------------------------------------------------------------------------
type EvaluatorM = EvaluatorM' Value
type EvaluatorM' a = StateT Env (ExceptT RuntimeException IO) a

class Evaluator a where

    eval :: a -> EvaluatorM


-- Logic ---------------------------------------------------------------------------------
instance Evaluator Program where

    eval (PProgram pos defs) = do
        mapM_ eval defs

        eval $ EApp pos (Ident "main") []

printEnv :: EvaluatorM
printEnv = do
    env <- get
    let vars = getEnv env
    liftIO $ print vars
    pure None


instance Evaluator TopDef where

    eval (PFnDef _ _ name args block) = do
        env <- get

        modify $ updateEnv name $ VFunc args block $ getEnv env

        pure None

    eval (PVarDef _ varType name) = do
        let val = getNullVal varType

        modify $ updateEnv name val

        pure None

    eval (PVarInit _ _ name expr) = do
        exprVal <- eval expr

        modify $ updateEnv name exprVal

        pure None


instance Evaluator Block where

    eval (SBlock _ stmts) = do
        mapM_ eval stmts

        pure None


instance Evaluator Stmt where

    eval (SEmpty _) = pure None

    eval (SBStmt _ block) = evalIfNotReturned $ do
        eval block

        pure None

    eval (STopDef _ topdef) = evalIfNotReturned $ do
        eval topdef

        pure None

    eval (SAss _ name expr) = evalIfNotReturned $ do
        exprVal <- eval expr

        modify $ updateEnv name exprVal

        pure None

    eval (SIncr _ name) = evalIfNotReturned $ do
        env <- get
        let vars = getEnv env

        let (Just (VInt exprVal)) = M.lookup name vars

        modify $ updateEnv name $ VInt $ exprVal + 1

        pure None

    eval (SDecr _ name) = evalIfNotReturned $ do
        env <- get
        let vars = getEnv env

        let (Just (VInt exprVal)) = M.lookup name vars

        modify $ updateEnv name $ VInt $ exprVal - 1

        pure None

    eval (SRet _ expr) = evalIfNotReturned $ do
        exprVal <- eval expr

        modify $ updateRetVal exprVal

        pure None

    eval (SRetVoid _)  = evalIfNotReturned $ do
        modify $ updateRetVal VVoid

        pure None

    eval (SCond _ cond block) = evalIfNotReturned $ do
        (VBool condVal) <- eval cond

        if condVal
            then eval block
            else pure None

        pure None

    eval (SCondElse _ cond blockTrue blockFalse) = evalIfNotReturned $ do
        (VBool condVal) <- eval cond

        if condVal
            then eval blockTrue
            else eval blockFalse

    eval while@(SWhile _ cond block) = evalIfNotReturned $ do
        (VBool condVal) <- eval cond

        if condVal
            then do
                eval block
                eval while
            else pure None


    eval (SBreak pos) = throwError $ UnimplementedException pos "Break"

    eval (SCont pos) = throwError $ UnimplementedException pos "Continue"

    eval (SExp _ expr) = evalIfNotReturned $ do
        eval expr

        pure None


instance Evaluator Expr where

    eval (EVar _ name) = gets $ getVarValue name

    eval (ELitInt _ val) = pure $ VInt val

    eval (ELitTrue _)  = pure $ VBool True

    eval (ELitFalse _) = pure $ VBool False

    eval funcApp@(EApp pos name args) = do
        argVals <- mapM eval args

        if name `elem` builtinFunction
            then evalBuiltinFunc $ head argVals
            else evalFuncApp pos name argVals

    eval (EString _ val) = pure $ VString val

    eval (ENeg _ expr) = do
        (VInt exprVal) <- eval expr
        pure $ VInt (-exprVal)

    eval (ENot _ expr) = do
        (VBool exprVal) <- eval expr
        pure $ VBool (not exprVal)

    eval (EMul pos expr1 op expr2) = do
        (VInt exprVal1) <- eval expr1
        (VInt exprVal2) <- eval expr2

        case op of
            OTimes _ -> pure $ VInt (exprVal1 * exprVal2)
            ODiv _   ->
                if exprVal2 == 0
                then throwError $ DivisionByZeroException pos
                else pure $ VInt (exprVal1 `div` exprVal2)
            OMod _   ->
                if exprVal2 == 0
                then throwError $ DivisionByZeroException pos
                else pure $ VInt (exprVal1 `mod` exprVal2)

    eval (EAdd _ expr1 op expr2) = do
        (VInt exprVal1) <- eval expr1
        (VInt exprVal2) <- eval expr2

        case op of
          OPlus _  -> pure $ VInt (exprVal1 + exprVal2)
          OMinus _ -> pure $ VInt (exprVal1 - exprVal2)

    eval (ERel _ expr1 op expr2) = do
        (VInt exprVal1) <- eval expr1
        (VInt exprVal2) <- eval expr2

        case op of
          OLth _ -> pure $ VBool (exprVal1 < exprVal2)
          OLE _  -> pure $ VBool (exprVal1 <= exprVal2)
          OGth _ -> pure $ VBool (exprVal1 > exprVal2)
          OGE _  -> pure $ VBool (exprVal1 >= exprVal2)
          OEq _  -> pure $ VBool (exprVal1 == exprVal2)
          ONe _  -> pure $ VBool (exprVal1 /= exprVal2)


    eval (EAnd _ expr1 expr2) = do
        (VBool exprVal1) <- eval expr1
        (VBool exprVal2) <- eval expr2
        pure $ VBool (exprVal1 && exprVal2)

    eval (EOr _ expr1 expr2) = do
        (VBool exprVal1) <- eval expr1
        (VBool exprVal2) <- eval expr2
        pure $ VBool (exprVal1 || exprVal2)


-- Utils ---------------------------------------------------------------------------------
evalIfNotReturned :: EvaluatorM -> EvaluatorM
evalIfNotReturned instr = do
    env <- get
    if getRetVal env == None
        then instr
        else pure None

evalBuiltinFunc :: Value -> EvaluatorM
evalBuiltinFunc val = do
    liftIO $ print val

    pure None

evalFuncApp :: BNFC'Position -> Ident -> [Value] -> EvaluatorM
evalFuncApp callPos name argVals = do
    env <- get
    let vars = getEnv env

    let func@(VFunc fArgs fBlock@(SBlock pos _) fEnv) = getVarValue name env
    let argIds = map getArgId fArgs

    modify $ insertEnv fEnv
    insertNameVal (name, func)
    mapM_ insertNameVal (zip argIds argVals)
    modify $ updateRetVal None

    eval fBlock

    env' <- get
    let retVal = getRetVal env'
    if retVal == None
        then throwError $ ReturnNotCalledException pos
        else pure ()

    put env

    pure retVal

getNullVal :: Type -> Value
getNullVal varType = case varType of
    TInt _    -> VInt 0
    TString _ -> VString ""
    TBool _   -> VBool False
    TVoid _   -> VVoid

getArgId :: Arg -> Ident
getArgId (PArg _  _ name)   = name
getArgId (PRefArg _ _ name) = name

insertNameVal :: (Ident, Value) -> EvaluatorM
insertNameVal (name, val) = do
    modify $ updateEnv name val

    pure None

insertEnv :: Env' -> Env -> Env
insertEnv env Env{..} = Env
    { env_ = env
    , retVal = retVal
    }


-- Entrypoint ----------------------------------------------------------------------------
evaluate :: Program -> IO (Either RuntimeException Value)
evaluate program = run
    where
        evalCall = eval program
        evalState = evalStateT evalCall initEnv
        run = runExceptT evalState
