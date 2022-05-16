-- File generated by the BNF Converter (bnfc 2.9.4).

{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}

-- | The abstract syntax of language Pepe.

module Pepe.Abs where

import           Prelude     (Integer, Maybe (Just), Show, String, show, (++))

import qualified Data.String
import qualified Prelude     as C (Eq, Foldable, Functor, Int, Maybe (..), Ord,
                                   Read, Show, Traversable)

type Program = Program' BNFC'Position
data Program' a = PProgram a [TopDef' a]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type TopDef = TopDef' BNFC'Position
data TopDef' a
    = PFnDef a (Type' a) Ident [Arg' a] (Block' a)
    | PVarInit a (Type' a) Ident (Expr' a)
    | PVarDef a (Type' a) Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Arg = Arg' BNFC'Position
data Arg' a = PArg a (Type' a) Ident | PRefArg a (Type' a) Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Block = Block' BNFC'Position
data Block' a = SBlock a [Stmt' a]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Stmt = Stmt' BNFC'Position
data Stmt' a
    = SEmpty a
    | SBStmt a (Block' a)
    | STopDef a (TopDef' a)
    | SAss a Ident (Expr' a)
    | SIncr a Ident
    | SDecr a Ident
    | SRet a (Expr' a)
    | SRetVoid a
    | SCond a (Expr' a) (Block' a)
    | SCondElse a (Expr' a) (Block' a) (Block' a)
    | SWhile a (Expr' a) (Block' a)
    | SBreak a
    | SCont a
    | SExp a (Expr' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Type = Type' BNFC'Position
data Type' a = TInt a | TString a | TBool a | TVoid a
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Expr = Expr' BNFC'Position
data Expr' a
    = EVar a Ident
    | ELitInt a Integer
    | ELitTrue a
    | ELitFalse a
    | EApp a Ident [Expr' a]
    | EString a String
    | ENeg a (Expr' a)
    | ENot a (Expr' a)
    | EMul a (Expr' a) (MulOp' a) (Expr' a)
    | EAdd a (Expr' a) (AddOp' a) (Expr' a)
    | ERel a (Expr' a) (RelOp' a) (Expr' a)
    | EAnd a (Expr' a) (Expr' a)
    | EOr a (Expr' a) (Expr' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type AddOp = AddOp' BNFC'Position
data AddOp' a = OPlus a | OMinus a
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type MulOp = MulOp' BNFC'Position
data MulOp' a = OTimes a | ODiv a | OMod a
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type RelOp = RelOp' BNFC'Position
data RelOp' a = OLth a | OLE a | OGth a | OGE a | OEq a | ONe a
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

newtype Ident = Ident String
  deriving (C.Eq, C.Ord, C.Read, Data.String.IsString)

instance Show Ident where
  show (Ident id) = show id

-- | Start position (line, column) of something.

type BNFC'Position = C.Maybe (C.Int, C.Int)

pattern BNFC'NoPosition :: BNFC'Position
pattern BNFC'NoPosition = C.Nothing

pattern BNFC'Position :: C.Int -> C.Int -> BNFC'Position
pattern BNFC'Position line col = C.Just (line, col)

-- | Get the start position of something.

class HasPosition a where
  hasPosition :: a -> BNFC'Position

instance HasPosition Program where
  hasPosition = \case
    PProgram p _ -> p

instance HasPosition TopDef where
  hasPosition = \case
    PFnDef p _ _ _ _ -> p
    PVarInit p _ _ _ -> p
    PVarDef p _ _    -> p

instance HasPosition Arg where
  hasPosition = \case
    PArg p _ _    -> p
    PRefArg p _ _ -> p

instance HasPosition Block where
  hasPosition = \case
    SBlock p _ -> p

instance HasPosition Stmt where
  hasPosition = \case
    SEmpty p          -> p
    SBStmt p _        -> p
    STopDef p _       -> p
    SAss p _ _        -> p
    SIncr p _         -> p
    SDecr p _         -> p
    SRet p _          -> p
    SRetVoid p        -> p
    SCond p _ _       -> p
    SCondElse p _ _ _ -> p
    SWhile p _ _      -> p
    SBreak p          -> p
    SCont p           -> p
    SExp p _          -> p

instance HasPosition Type where
  hasPosition = \case
    TInt p    -> p
    TString p -> p
    TBool p   -> p
    TVoid p   -> p

instance HasPosition Expr where
  hasPosition = \case
    EVar p _     -> p
    ELitInt p _  -> p
    ELitTrue p   -> p
    ELitFalse p  -> p
    EApp p _ _   -> p
    EString p _  -> p
    ENeg p _     -> p
    ENot p _     -> p
    EMul p _ _ _ -> p
    EAdd p _ _ _ -> p
    ERel p _ _ _ -> p
    EAnd p _ _   -> p
    EOr p _ _    -> p

instance HasPosition AddOp where
  hasPosition = \case
    OPlus p  -> p
    OMinus p -> p

instance HasPosition MulOp where
  hasPosition = \case
    OTimes p -> p
    ODiv p   -> p
    OMod p   -> p

instance HasPosition RelOp where
  hasPosition = \case
    OLth p -> p
    OLE p  -> p
    OGth p -> p
    OGE p  -> p
    OEq p  -> p
    ONe p  -> p

showPos :: BNFC'Position -> String
showPos (Just (line, col)) = "line: " ++ show line ++ ", col: " ++ show col
showPos _                  = "undefined position"
