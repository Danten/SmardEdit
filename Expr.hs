module Expr where

import Types
import Text.PrettyPrint.ANSI.Leijen

data Program
    = PDef Def
    | PExpr Expr
    | PType Type
    | PBind Bind
    deriving (Eq, Show)

data Def
    = Module 
    | Def 
    deriving (Eq, Show)

data Type
    = TVar Id
    | TFun
    deriving (Eq, Show)

data Expr
    = Lambda 
    | App
    | Var Id
    deriving (Eq, Show)

data Bind
    = Single Id
    | Multiple
    deriving (Eq, Show)

data MFocus
    = MDef | MExpr | MType | MBind | None
    deriving (Eq, Show)

type Id = String

currentMFocus :: Context Program -> MFocus
currentMFocus ([], _) = MDef
currentMFocus (InvNode (PDef d) pr ne : cs, _) = case d of
    Module -> case pr of
        []  -> MBind
        [b] -> MDef
        _ -> None
    Def -> case pr of
        []    -> MBind
        [b]   -> MType
        [b,e] -> MExpr
        _ -> None
currentMFocus (InvNode (PExpr e) pr ne : cs, _) = case e of
    Lambda -> case pr of
        [] -> MBind
        [b] -> MExpr
        _ -> None
    App -> MExpr
    Var _ -> None
currentMFocus (InvNode (PBind b) pr ne : cs, _) = MBind
currentMFocus _ = None

isFlexible :: InvTree Program -> (Bool, Bool)
isFlexible (InvNode (PDef d) pr ne) = case d of
    Module -> case pr of
        []    -> (True, False)
        _:xs  -> (True, not $ null xs && null ne)
    Def -> (False, False)
isFlexible (InvNode (PExpr e) pr ne) = case e of
    Lambda -> (False, False)
    App    -> (True, not $ null pr && null ne)
    Var _  -> (False, False)
isFlexible (InvNode (PBind b) pr ne) = case b of
    Single _ -> (False, False)
    Multiple -> (True, not $ null pr && null ne)
isFlexible (InvNode (PType t) pr ne) = case t of
    TVar _ -> (False, False)
    TFun   -> (True, not $ null pr && null ne)

prProgram :: Program -> [Doc] -> Doc
prProgram (PDef d)  = prDef d
prProgram (PExpr e) = prExpr e
prProgram (PType t) = prType t
prProgram (PBind b) = prBind b

prDef :: Def -> [Doc] -> Doc
prDef Module = \(b:ds) -> parens $ 
    prKey "module" <+> b <> line <> indent 2 (vsep ds)
prDef Def = \[b, t, e] -> parens $
    prKey "define" <+> b <+> t <> line <> indent 2 e

prType :: Type -> [Doc] -> Doc
prType (TVar id) _ = prId id
prType TFun args = parens $ prKey "->" <+> hsep  args

prExpr :: Expr -> [Doc] -> Doc
prExpr Lambda = \[b,e] -> parens $
    prKey "lambda" <+> b <> line <> indent 2 e
prExpr (App) = \es -> parens $ hsep es
prExpr (Var id) = \[] -> prId id

prBind :: Bind -> [Doc] -> Doc
prBind (Single id) = \[] -> prId id
prBind Multiple = parens . hsep

prId :: Id -> Doc
prId = green . text

prKey :: String -> Doc
prKey = blue . bold . text

