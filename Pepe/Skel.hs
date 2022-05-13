module Pepe.Skel where


import           Pepe.Abs
import           Pepe.ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
    Ident string -> failure x
transProgram :: Show a => Program a -> Result
transProgram x = case x of
    PProgram _ topdefs -> failure x
transTopDef :: Show a => TopDef a -> Result
transTopDef x = case x of
    PFnDef _ type_ ident args block -> failure x
    PVarDef _ type_ ident expr      -> failure x
transArg :: Show a => Arg a -> Result
transArg x = case x of
    PArg _ type_ ident    -> failure x
    PRefArg _ type_ ident -> failure x
transBlock :: Show a => Block a -> Result
transBlock x = case x of
    SBlock _ stmts -> failure x
transStmt :: Show a => Stmt a -> Result
transStmt x = case x of
    SEmpty _                       -> failure x
    SBStmt _ block                 -> failure x
    STopDef _ topdef               -> failure x
    SDecl _ type_ items            -> failure x
    SAss _ ident expr              -> failure x
    SIncr _ ident                  -> failure x
    SDecr _ ident                  -> failure x
    SRet _ expr                    -> failure x
    SRetVoid _                     -> failure x
    SCond _ expr block             -> failure x
    SCondElse _ expr block1 block2 -> failure x
    SWhile _ expr block            -> failure x
    SBreak _                       -> failure x
    SCont _                        -> failure x
    SExp _ expr                    -> failure x
transItem :: Show a => Item a -> Result
transItem x = case x of
    SNoInit _ ident    -> failure x
    SInit _ ident expr -> failure x
transType :: Show a => Type a -> Result
transType x = case x of
    TInt _    -> failure x
    TString _ -> failure x
    TBool _   -> failure x
    TVoid _   -> failure x
transExpr :: Show a => Expr a -> Result
transExpr x = case x of
    EVar _ ident             -> failure x
    ELitInt _ integer        -> failure x
    ELitTrue _               -> failure x
    ELitFalse _              -> failure x
    EApp _ ident exprs       -> failure x
    EString _ string         -> failure x
    ENeg _ expr              -> failure x
    ENot _ expr              -> failure x
    EMul _ expr1 mulop expr2 -> failure x
    EAdd _ expr1 addop expr2 -> failure x
    ERel _ expr1 relop expr2 -> failure x
    EAnd _ expr1 expr2       -> failure x
    EOr _ expr1 expr2        -> failure x
transAddOp :: Show a => AddOp a -> Result
transAddOp x = case x of
    OPlus _  -> failure x
    OMinus _ -> failure x
transMulOp :: Show a => MulOp a -> Result
transMulOp x = case x of
    OTimes _ -> failure x
    ODiv _   -> failure x
    OMod _   -> failure x
transRelOp :: Show a => RelOp a -> Result
transRelOp x = case x of
    OLth _ -> failure x
    OLE _  -> failure x
    OGth _ -> failure x
    OGE _  -> failure x
    OEq _  -> failure x
    ONe _  -> failure x