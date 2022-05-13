module Pepe.Abs where


newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
data Program a = PProgram a [TopDef a]
    deriving (Eq, Ord, Show, Read)

instance Functor Program where
    fmap f x = case x of
        PProgram a topdefs -> PProgram (f a) (map (fmap f) topdefs)
data TopDef a
    = PFnDef a (Type a) Ident [Arg a] (Block a)
    | PVarDef a (Type a) Ident (Expr a)
  deriving (Eq, Ord, Show, Read)

instance Functor TopDef where
    fmap f x = case x of
        PFnDef a type_ ident args block -> PFnDef (f a) (fmap f type_) ident (map (fmap f) args) (fmap f block)
        PVarDef a type_ ident expr -> PVarDef (f a) (fmap f type_) ident (fmap f expr)
data Arg a = PArg a (Type a) Ident | PRefArg a (Type a) Ident
  deriving (Eq, Ord, Show, Read)

instance Functor Arg where
    fmap f x = case x of
        PArg a type_ ident    -> PArg (f a) (fmap f type_) ident
        PRefArg a type_ ident -> PRefArg (f a) (fmap f type_) ident
data Block a = SBlock a [Stmt a]
  deriving (Eq, Ord, Show, Read)

instance Functor Block where
    fmap f x = case x of
        SBlock a stmts -> SBlock (f a) (map (fmap f) stmts)
data Stmt a
    = SEmpty a
    | SBStmt a (Block a)
    | STopDef a (TopDef a)
    | SDecl a (Type a) [Item a]
    | SAss a Ident (Expr a)
    | SIncr a Ident
    | SDecr a Ident
    | SRet a (Expr a)
    | SRetVoid a
    | SCond a (Expr a) (Block a)
    | SCondElse a (Expr a) (Block a) (Block a)
    | SWhile a (Expr a) (Block a)
    | SBreak a
    | SCont a
    | SExp a (Expr a)
  deriving (Eq, Ord, Show, Read)

instance Functor Stmt where
    fmap f x = case x of
        SEmpty a -> SEmpty (f a)
        SBStmt a block -> SBStmt (f a) (fmap f block)
        STopDef a topdef -> STopDef (f a) (fmap f topdef)
        SDecl a type_ items -> SDecl (f a) (fmap f type_) (map (fmap f) items)
        SAss a ident expr -> SAss (f a) ident (fmap f expr)
        SIncr a ident -> SIncr (f a) ident
        SDecr a ident -> SDecr (f a) ident
        SRet a expr -> SRet (f a) (fmap f expr)
        SRetVoid a -> SRetVoid (f a)
        SCond a expr block -> SCond (f a) (fmap f expr) (fmap f block)
        SCondElse a expr block1 block2 -> SCondElse (f a) (fmap f expr) (fmap f block1) (fmap f block2)
        SWhile a expr block -> SWhile (f a) (fmap f expr) (fmap f block)
        SBreak a -> SBreak (f a)
        SCont a -> SCont (f a)
        SExp a expr -> SExp (f a) (fmap f expr)
data Item a = SNoInit a Ident | SInit a Ident (Expr a)
  deriving (Eq, Ord, Show, Read)

instance Functor Item where
    fmap f x = case x of
        SNoInit a ident    -> SNoInit (f a) ident
        SInit a ident expr -> SInit (f a) ident (fmap f expr)
data Type a = TInt a | TString a | TBool a | TVoid a
  deriving (Eq, Ord, Show, Read)

instance Functor Type where
    fmap f x = case x of
        TInt a    -> TInt (f a)
        TString a -> TString (f a)
        TBool a   -> TBool (f a)
        TVoid a   -> TVoid (f a)
data Expr a
    = EVar a Ident
    | ELitInt a Integer
    | ELitTrue a
    | ELitFalse a
    | EApp a Ident [Expr a]
    | EString a String
    | ENeg a (Expr a)
    | ENot a (Expr a)
    | EMul a (Expr a) (MulOp a) (Expr a)
    | EAdd a (Expr a) (AddOp a) (Expr a)
    | ERel a (Expr a) (RelOp a) (Expr a)
    | EAnd a (Expr a) (Expr a)
    | EOr a (Expr a) (Expr a)
  deriving (Eq, Ord, Show, Read)

instance Functor Expr where
    fmap f x = case x of
        EVar a ident -> EVar (f a) ident
        ELitInt a integer -> ELitInt (f a) integer
        ELitTrue a -> ELitTrue (f a)
        ELitFalse a -> ELitFalse (f a)
        EApp a ident exprs -> EApp (f a) ident (map (fmap f) exprs)
        EString a string -> EString (f a) string
        ENeg a expr -> ENeg (f a) (fmap f expr)
        ENot a expr -> ENot (f a) (fmap f expr)
        EMul a expr1 mulop expr2 -> EMul (f a) (fmap f expr1) (fmap f mulop) (fmap f expr2)
        EAdd a expr1 addop expr2 -> EAdd (f a) (fmap f expr1) (fmap f addop) (fmap f expr2)
        ERel a expr1 relop expr2 -> ERel (f a) (fmap f expr1) (fmap f relop) (fmap f expr2)
        EAnd a expr1 expr2 -> EAnd (f a) (fmap f expr1) (fmap f expr2)
        EOr a expr1 expr2 -> EOr (f a) (fmap f expr1) (fmap f expr2)
data AddOp a = OPlus a | OMinus a
  deriving (Eq, Ord, Show, Read)

instance Functor AddOp where
    fmap f x = case x of
        OPlus a  -> OPlus (f a)
        OMinus a -> OMinus (f a)
data MulOp a = OTimes a | ODiv a | OMod a
  deriving (Eq, Ord, Show, Read)

instance Functor MulOp where
    fmap f x = case x of
        OTimes a -> OTimes (f a)
        ODiv a   -> ODiv (f a)
        OMod a   -> OMod (f a)
data RelOp a = OLth a | OLE a | OGth a | OGE a | OEq a | ONe a
  deriving (Eq, Ord, Show, Read)

instance Functor RelOp where
    fmap f x = case x of
        OLth a -> OLth (f a)
        OLE a  -> OLE (f a)
        OGth a -> OGth (f a)
        OGE a  -> OGE (f a)
        OEq a  -> OEq (f a)
        ONe a  -> ONe (f a)
