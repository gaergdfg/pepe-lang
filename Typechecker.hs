{-# LANGUAGE FlexibleInstances #-}
module Typechecker (typecheck) where

import           Control.Monad.Except   (Except, MonadError (throwError),
                                         runExcept)
import           Control.Monad.Identity ()
import           Control.Monad.Reader   (MonadReader (ask),
                                         ReaderT (runReaderT))
import           Control.Monad.State    (MonadState (get, put), StateT,
                                         evalStateT)
import           Data.List              (intercalate, nub, (\\))

import           Pepe.Abs               (Arg, Arg' (..), BNFC'Position, Block,
                                         Block' (..), Expr, Expr' (..),
                                         Ident (..), Program, Program' (..),
                                         Stmt, Stmt' (..), TopDef, TopDef' (..),
                                         Type, Type' (..), showPos)
import           Pepe.ErrM              ()

import qualified Data.Map               as M


-- Types ---------------------------------------------------------------------------------
data RawType
    = RTBool
    | RTInt
    | RTString
    | RTVoid
    | RTFunc [RawType] RawType
    deriving (Eq)

instance Show RawType where

    show RTBool   = "bool"

    show RTInt    = "int"

    show RTString = "string"

    show RTVoid   = "void"

    show (RTFunc argTypes resType) =
        concat
        [ "Function ["
        , intercalate ", " $ map show argTypes
        , "] -> "
        , show resType
        ]


getRawType :: Type -> RawType
getRawType (TBool _)   = RTBool
getRawType (TInt _ )   = RTInt
getRawType (TString _) = RTString
getRawType (TVoid _ )  = RTVoid


-- Exceptions ----------------------------------------------------------------------------
type TypecheckException = TypecheckException' BNFC'Position

data TypecheckException' a
    = UnimplementedException a String
    | InvalidCallException a RawType
    | InvalidFunctionArgTypesException a Ident [RawType] [RawType]
    | RedefinitionException a Ident
    | TypeMismatchException a RawType RawType
    | UnexpectedReturnException a
    | UndefinedSymbolException a Ident


instance Show TypecheckException where

    show (UnimplementedException pos msg) =
        concat
        [ "Error - unimplemented functionality! "
        , msg
        , ", at :"
        , showPos pos
        ]

    show (InvalidCallException pos rawType) =
        concat
        [ "Error - invalid attempt to call a variable of type: "
        , show rawType
        , ", at: "
        , showPos pos
        ]

    show (InvalidFunctionArgTypesException pos name expectedTypes realTypes) =
        concat
        [ "Error - invalid function call argument types!"
        , " Tried calling "
        , show name
        , ", at: "
        , showPos pos
        , "\nExpected: ["
        , intercalate ", " $ map show expectedTypes
        , "]\nGot: ["
        , intercalate ", " $ map show realTypes
        , "]"
        ]

    show (RedefinitionException pos name) =
        concat
        [ "Error - redefinition of: "
        , show name
        , ", at: "
        , showPos pos
        ]

    show (TypeMismatchException pos expectedType realType) =
        concat
        [ "Error - type mismatch at:"
        , showPos pos
        , "\nExpected: "
        , show expectedType
        , "\nGot: "
        , show realType
        ]

    show (UnexpectedReturnException pos) =
        "Error - unexpected return statement at: " ++ showPos pos

    show (UndefinedSymbolException pos name) =
        concat
        [ "Error - undefined symbol: "
        , show name
        , ", at: "
        , showPos pos
        ]


-- Environment ---------------------------------------------------------------------------
type Env = M.Map Ident RawType

builtinFunctions :: [(Ident, RawType)]
builtinFunctions =
    [ (Ident "printBool", RTFunc [RTBool] RTVoid)
    , (Ident "printInt", RTFunc [RTInt] RTVoid)
    , (Ident "printString", RTFunc [RTString] RTVoid)
    ]

initEnv :: Env
initEnv = M.fromList builtinFunctions


-- Monads --------------------------------------------------------------------------------
type TypecheckerM = TypecheckerM' ()
type TypecheckerM' a = StateT Env (Except TypecheckException) a

type TypegetterM = TypegetterM' RawType
type TypegetterNullM = TypegetterM' ()
type TypegetterM' a = ReaderT Env (Except TypecheckException) a

class Typechecker a where

    checkType :: Maybe RawType -> a -> TypecheckerM

class Typegetter a where

    getType :: a -> TypegetterM


-- Logic ---------------------------------------------------------------------------------
instance Typechecker Program where

    checkType _ (PProgram pos defs) = do
        ensureNoDuplicateIdentsP pos defs
        mapM_ (checkType Nothing) defs

        ensureVarIsOfTypeTC (Just (0, 0)) (Ident "main") (RTFunc [] RTInt)


instance Typechecker TopDef where

    checkType _ (PFnDef pos resType name args block@(SBlock _ stmts)) = do
        ensureNoDuplicateIdentsF pos args stmts

        env <- get

        let rawResType = getRawType resType
        let argNameTypes = map getArgTypeName args
        let argTypes = map snd argNameTypes
        let envWithFunction = insertType env name (RTFunc argTypes rawResType)
        let blockEnv = insertTypes envWithFunction argNameTypes

        put blockEnv

        checkType (Just rawResType) block

        put envWithFunction

    checkType _ (PVarInit pos varType name expr) = do
        env <- get

        let rawVarType = getRawType varType
        let rawExprType = resolveType pos env expr
        assertMaybeTypeMatchesTC pos rawVarType rawExprType

        put $ insertType env name rawVarType

    checkType _ (PVarDef pos varType name) = do
        env <- get

        let rawVarType = getRawType varType

        put $ insertType env name rawVarType


instance Typechecker Block where

    checkType expectedType (SBlock pos stmts) = mapM_ (checkType expectedType) stmts


instance Typechecker Stmt where

    checkType _ (SEmpty _) = pure ()

    checkType expectedType (SBStmt _ block) = do
        env <- get

        checkType expectedType block

        put env

    checkType expectedType (STopDef _ topdef) = checkType expectedType topdef

    checkType _ (SAss pos name expr) = do
        env <- get

        case M.lookup name env of
            Nothing      -> throwError $ UndefinedSymbolException pos name
            Just rawType ->
                assertMaybeTypeMatchesTC pos rawType $ resolveType pos env expr

    checkType _ (SIncr pos name) = ensureVarIsOfTypeTC pos name RTInt

    checkType _ (SDecr pos name) = ensureVarIsOfTypeTC pos name RTInt

    checkType (Just expectedType) (SRet pos expr) = do
        env <- get

        assertMaybeTypeMatchesTC pos expectedType $ resolveType pos env expr

    checkType Nothing (SRet pos _) = throwError $ UnexpectedReturnException pos

    checkType (Just expectedType) (SRetVoid pos) =
        assertTypesMatchTC pos RTVoid expectedType

    checkType Nothing (SRetVoid pos) = throwError $ UnexpectedReturnException pos

    checkType expectedType (SCond pos cond block) = do
        env <- get

        assertMaybeTypeMatchesTC pos RTBool $ resolveType pos env cond
        checkType expectedType block

    checkType expectedType (SCondElse pos cond blockTrue blockFalse) = do
        env <- get

        assertMaybeTypeMatchesTC pos RTBool $ resolveType pos env cond
        checkType expectedType blockTrue
        checkType expectedType blockFalse

    checkType expectedType (SWhile pos cond block) = do
        env <- get

        assertMaybeTypeMatchesTC pos RTBool $ resolveType pos env cond
        checkType expectedType block

    checkType _ (SBreak pos) = throwError $ UnimplementedException pos "Break"

    checkType _ (SCont pos) = throwError $ UnimplementedException pos "Continue"

    checkType _ (SExp pos expr) = do
        env <- get

        case resolveType pos env expr of
          Left error -> throwError error
          Right _    -> pure ()


instance Typegetter Expr where

    getType (EVar pos name) = do
        env <- ask

        case M.lookup name env of
            Nothing        -> throwError $ UndefinedSymbolException pos name
            (Just rawType) -> pure rawType

    getType (ELitInt pos _) = pure RTInt

    getType (ELitTrue pos)  = pure RTBool

    getType (ELitFalse pos) = pure RTBool

    getType (EApp pos name args) = do
        env <- ask

        case M.lookup name env of
            Nothing -> throwError $ UndefinedSymbolException pos name
            Just rawType@(RTFunc argTypes resType) -> do
                rawArgTypes <- mapM getType args
                assertTG
                    (argTypes == rawArgTypes)
                    $ InvalidFunctionArgTypesException pos name argTypes rawArgTypes
                pure resType
            Just rawType -> throwError $ InvalidCallException pos rawType

    getType (EString pos _) = pure RTString

    getType (ENeg pos expr) = do
        ensureExprType pos RTInt expr
        pure RTInt

    getType (ENot pos expr) = do
        ensureExprType pos RTBool expr
        pure RTBool

    getType (EMul pos expr1 _ expr2) = do
        ensureExprTypes pos RTInt expr1 expr2
        pure RTInt

    getType (EAdd pos expr1 _ expr2) = do
        ensureExprTypes pos RTInt expr1 expr2
        pure RTInt

    getType (ERel pos expr1 _ expr2) = do
        ensureExprTypes pos RTInt expr1 expr2
        pure RTBool

    getType (EAnd pos expr1 expr2) = do
        ensureExprTypes pos RTBool expr1 expr2
        pure RTBool

    getType (EOr pos expr1 expr2) = do
        ensureExprTypes pos RTBool expr1 expr2
        pure RTBool


resolveType :: Typegetter a => b -> Env -> a -> Either TypecheckException RawType
resolveType pos env expr = run
    where
        typeResolveCall = getType expr
        runReader = runReaderT typeResolveCall env
        run = runExcept runReader


-- Utils ---------------------------------------------------------------------------------
-- typechecker --
assertTC :: Bool -> TypecheckException -> TypecheckerM
assertTC True _          = pure ()
assertTC False exception = throwError exception

assertTypesMatchTC :: BNFC'Position -> RawType -> RawType -> TypecheckerM
assertTypesMatchTC pos expectedType realType = assertTC
    (expectedType == realType)
    (TypeMismatchException pos expectedType realType)

assertMaybeTypeMatchesTC ::
    BNFC'Position -> RawType -> Either TypecheckException RawType -> TypecheckerM
assertMaybeTypeMatchesTC _ _ (Left exception) = throwError exception
assertMaybeTypeMatchesTC pos expectedType (Right realType) =
    assertTypesMatchTC pos expectedType realType

ensureVarIsOfTypeTC :: BNFC'Position -> Ident -> RawType -> TypecheckerM
ensureVarIsOfTypeTC pos name expectedType = do
    env <- get

    case M.lookup name env of
        Nothing      -> throwError $ UndefinedSymbolException pos name
        Just rawType -> assertTypesMatchTC pos expectedType rawType

-- typegetter --
assertTG :: Bool -> TypecheckException -> TypegetterNullM
assertTG True _          = pure ()
assertTG False exception = throwError exception

assertTypesMatchTG ::
    BNFC'Position -> RawType -> Either TypecheckException RawType -> TypegetterNullM
assertTypesMatchTG _ _ (Left exception) = throwError exception
assertTypesMatchTG pos expectedType (Right realType) = assertTG
    (expectedType == realType)
    (TypeMismatchException pos expectedType realType)

ensureExprType :: Typegetter a => BNFC'Position -> RawType -> a -> TypegetterNullM
ensureExprType pos expectedType expr = do
    env <- ask

    let realType = resolveType pos env expr

    assertTypesMatchTG pos expectedType realType

ensureExprTypes :: Typegetter a => BNFC'Position -> RawType -> a -> a -> TypegetterNullM
ensureExprTypes pos expectedType expr1 expr2 = do
    env <- ask

    let realType1 = resolveType pos env expr1
    let realType2 = resolveType pos env expr2

    assertTypesMatchTG pos expectedType realType1
    assertTypesMatchTG pos expectedType realType2

getArgTypeName :: Arg -> (Ident, RawType)
getArgTypeName (PArg _ aType name)    = (name, getRawType aType)
getArgTypeName (PRefArg _ aType name) = (name, getRawType aType)

insertType :: Env -> Ident -> RawType -> Env
insertType env id rType = M.insert id rType env

insertTypePair :: Env -> (Ident, RawType) -> Env
insertTypePair env (id, rType) = insertType env id rType

insertTypes :: Env -> [(Ident, RawType)] -> Env
insertTypes = foldl insertTypePair

-- duplicate declarations --
ensureNoDuplicateIdentsP :: BNFC'Position -> [TopDef] -> TypecheckerM
ensureNoDuplicateIdentsP pos defs =
    ensureNoDuplicateIdents pos $ map getTopDefId defs

ensureNoDuplicateIdentsF :: BNFC'Position -> [Arg] -> [Stmt] -> TypecheckerM
ensureNoDuplicateIdentsF pos args block =
    ensureNoDuplicateIdents pos $ argIds ++ blockIds
    where
        argIds = map getArgId args
        blockIds = extractDefs block

extractDefs :: [Stmt] -> [Ident]
extractDefs block = filter f list
    where
        f = \id -> id /= Ident ""
        list = foldr extractDefsAcc [] block

extractDefsAcc :: Stmt -> [Ident] -> [Ident]
extractDefsAcc stmt acc  = extractDef stmt : acc

extractDef :: Stmt -> Ident
extractDef (STopDef _ def) = case def of
    (PFnDef _ _ name _ __) -> name
    (PVarInit _ _ name _ ) -> name
    (PVarDef _ _ name)     -> name
extractDef _ = Ident ""


ensureNoDuplicateIdents :: BNFC'Position -> [Ident] -> TypecheckerM
ensureNoDuplicateIdents pos list =
    if null duplicates
    then pure ()
    else throwError $ RedefinitionException pos $ head duplicates
    where
        noDupList = nub list
        duplicates = list \\ noDupList

getTopDefId :: TopDef -> Ident
getTopDefId (PFnDef _ _ name _ _) = name
getTopDefId (PVarInit _ _ name _) = name
getTopDefId (PVarDef _ _ name)    = name

getArgId :: Arg -> Ident
getArgId (PArg _  _ name)   = name
getArgId (PRefArg _ _ name) = name


-- Entrypoint ----------------------------------------------------------------------------
typecheck :: Program -> Either TypecheckException ()
typecheck program = run
    where
        typecheckCall = checkType Nothing program
        evalState = evalStateT typecheckCall initEnv
        run = runExcept evalState
