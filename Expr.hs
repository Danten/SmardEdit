module Expr where

import Text.PrettyPrint.ANSI.Leijen

data Program
    = PDef Def
    | PExpr Expr
    deriving (Eq, Show)

data Def
    = Module Id
    | Def Id
    deriving (Eq, Show)

data Expr
    = Lambda Id
    | App
    | Var Id
    deriving (Eq, Show)

data Mode
    = MDef | MExpr
    deriving (Eq, Show)

type Id = String

prProgram :: Program -> [Doc] -> Doc
prProgram (PDef d)  = prDef d
prProgram (PExpr e) = prExpr e

prDef :: Def ->[Doc] -> Doc
prDef (Module id) = \ds -> parens $ 
    prKey "module" <+> prId id <> line <> indent 2 (vsep ds)
prDef (Def id) = \[e] -> parens $
    prKey "define" <+> prId id <> line <> indent 2 e

prExpr :: Expr -> [Doc] -> Doc
prExpr (Lambda id) = \[e] -> parens $
    prKey "lambda" <+> prId id <> line <> indent 2 e
prExpr (App) = \es -> parens $ hsep es
prExpr (Var id) = \[] -> prId id

prId :: Id -> Doc
prId = green . text

prKey :: String -> Doc
prKey = blue . bold . text

