module Eval where

import Control.Applicative
import Control.Monad

import Types
import Expr

{-

eval :: Tree Program -> Maybe (Tree Program)
eval (Node (PExpr e) cs) = case e of
    App -> case cs of
        Node (PExpr (Lambda x)) [e] : c : cs -> if null cs
            then subst c x e
            else do
                x <- subst c x e
                return $ Node (PExpr App) (x:cs)
        x : c : cs -> do
            x' <- eval x
            return $ Node (PExpr App) (x':c:cs)
        _ -> mzero
    _ -> mzero
eval _ = mzero

subst :: Tree Program -> Id -> Tree Program -> Maybe (Tree Program)
subst to id from = case from of
    Node (PExpr (Var id')) [] 
        | id == id' -> return to
        | otherwise -> return from
    Node (PExpr App) cs -> do
        cs' <- mapM (subst to id) cs
        return $ Node (PExpr App) cs'
    Node (PExpr (Lambda id')) [e]
        | id == id' -> return from
        | id `elem` free to -> mzero
        | otherwise -> Node (PExpr (Lambda id')) . (:[]) <$> subst to id e

free :: Tree Program -> [Id]
free (Node (PExpr e) args) = case e of
    Var id -> [id]
    App -> args >>= free
    Lambda i -> filter (/=i) $ args >>= free
-}  
