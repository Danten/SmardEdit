module Pretty where

import Text.PrettyPrint.ANSI.Leijen
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Data.Maybe
import Control.Arrow ((***))

import Simple
import Moves


prExpr :: Expr -> Int -> Doc
prExpr expr l = case expr of
    ELit lit       -> prLit lit
    EFocus e       -> brackets (prExpr e 0)
    EHole          -> prHole
    EVar ident     -> prVar ident 
    EBin op e1 e2  -> par 2 $ prExpr e1 2 <+> text op <+>  prExpr e2 2  --prBin ident exp1 exp2
    EApp e1 e2     -> par 3 $ prExpr e1 2 <+> prExpr e2 3 --prApp exp1 exp2 
    EAbs x e       -> par 1 $ text "\\" <> text x <+> prExpr e 0 --prAbs ident exp
  where
    par g | l >= g     = parens
          | otherwise = id

prLit :: Lit -> Doc
prLit (LInt v)  = PP.int v
prLit (LBool v) = bool v

prHole :: Doc
prHole = char '?'

prVar :: Ident -> Doc
prVar x = text x

prExprCtx :: ExprCtx -> Doc
prExprCtx i = prExprCtx' $ (id *** EFocus) i
  where 
    prExprCtx' i = case moveUp i of
        Nothing -> prExpr (snd i) 0
        Just x  -> prExprCtx' x
    