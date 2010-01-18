{-# LANGUAGE GADTs #-}
module Types where 

import Control.Applicative
import Control.Monad
import Data.Map

data Def
    = Module Id [Def]
    | ValDef Id Expr
    | DHole | DFocus Def
    deriving(Eq, Show)

data Expr
    = Lambda Id Expr
    | Case Expr [(Pat, Expr)]
    | App [Expr]
    | Var Id
    | EHole | EFocus Expr
    deriving (Eq, Show)

data Pat
    = PVar Id
    | PApp [Pat]
    | PHole | PFocus Pat
    deriving (Eq, Show)

type Id = Integer

type Env = Map Id String

data Step
    = FromModule Id [Def] [Def]
    | FromDef Id
    | FromLambda Id
    | FromCaseScrut [(Pat, Expr)]
    | FromCasePat Expr [(Pat, Expr)] Expr [(Pat, Expr)]
    | FromCaseExpr Expr [(Pat, Expr)] Pat [(Pat, Expr)]
    | FromApp [Expr] [Expr]
    | FromPApp [Pat] [Pat]
    deriving (Eq, Show)

type Trace = [Step]

data Focus
    = FDef  Def
    | FExpr Expr
    | FPat  Pat
    deriving (Eq, Show)

type Context = (Trace, Focus)

getDef :: Focus -> Maybe Def
getDef (FDef d) = return d
getDef _        = mzero

getExpr :: Focus -> Maybe Expr
getExpr (FExpr e) = return e
getExpr _         = mzero

getPat :: Focus -> Maybe Pat
getPat (FPat p) = return p
getPat _        = mzero

bet :: [a] -> [a] -> a -> [a]
bet pr ne x = reverse pr ++ x : ne

moveUp :: Context -> Maybe Context
moveUp ([], _)      = Nothing
moveUp (c : cs , f) = (,) cs <$> case c of
    FromModule id pr ne -> FDef . Module id . bet pr ne <$> getDef f
    FromDef id -> FDef . ValDef id <$> getExpr f
    FromLambda id -> FExpr . Lambda id <$> getExpr f
    FromCaseScrut pats -> FExpr . flip Case pats <$> getExpr f
    FromCaseExpr scrut pr pat ne 
        -> FExpr . Case scrut . bet pr ne . (,) pat <$> getExpr f
    FromCasePat scrut pr exp ne
        -> FExpr . Case scrut . bet pr ne . flip (,) exp <$> getPat f
    FromApp pr ne -> FExpr . App . bet pr ne <$> getExpr f
    FromPApp pr ne -> FPat . PApp . bet pr ne <$> getPat f

moveDown :: Context -> Maybe Context
moveDown (cs, FDef d) = case d of
    Module id (d : ds) -> return (FromModule id [] ds : cs, FDef d)
    ValDef id e -> return (FromDef id : cs, FExpr e)
    _ -> mzero
moveDown (cs, FExpr e) = case e of
    Lambda id e -> return (FromLambda id : cs, FExpr e)
    Case scrut pats -> return (FromCaseScrut pats : cs, FExpr scrut)
    App (e : es) -> return (FromApp [] es : cs, FExpr e)
    _ -> mzero
moveDown (cs, FPat p) = case p of
    PApp (p : ps) -> return (FromPApp [] ps : cs, FPat p)
    _ -> mzero

moveLeft :: Context -> Maybe Context
moveLeft ([], _) = Nothing
moveLeft (c : cs, f) = case c of
    FromModule id (d:pr) ne 
        -> flip (,) (FDef d) . (:cs) . FromModule id pr . (:ne) <$> getDef f
    FromCasePat scrut [] exp ne -> do
        pat <- getPat f
        return (FromCaseScrut ((pat, exp) : ne) : cs, FExpr scrut)
    FromCasePat scrut ((p,e) :pr) exp ne -> do
        pat <- getPat f
        return (FromCaseExpr scrut pr p ((pat,exp):ne) :cs, FExpr e)
    FromCaseExpr scrut pr pat ne -> do
        exp <- getExpr f
        return (FromCasePat scrut pr exp ne : cs, FPat pat)
    FromApp (e : pr) ne -> do
        exp <- getExpr f
        return (FromApp pr (exp : ne) : cs, FExpr e)
    FromPApp (p : pr) ne -> do
        pat <- getPat f
        return (FromPApp pr (pat : ne) : cs, FPat p)
    _ -> mzero
        
moveRight :: Context -> Maybe Context
moveRight ([], _) = Nothing
moveRight (c : cs, f) = case c of
    FromModule id pr (d:ne) -> do
        def <- getDef f
        return (FromModule id (def : pr) ne : cs, FDef d)
    FromCaseScrut ((p,e) : pats) -> do
        scrut <- getExpr f
        return (FromCasePat scrut [] e pats : cs, FPat p) 
    FromCasePat scrut pr exp ne -> do
        pat <- getPat f
        return (FromCaseExpr scrut pr pat ne :cs, FExpr exp)
    FromCaseExpr scrut pr pat ((p, e):ne) -> do
        exp <- getExpr f
        return (FromCasePat scrut ((pat,exp):pr) e ne : cs, FPat p)
    FromApp pr (e : ne) -> do
        exp <- getExpr f
        return (FromApp (exp:pr) ne : cs, FExpr e)
    FromPApp pr (p : ne) -> do
        pat <- getPat f
        return (FromPApp (pat:pr) ne : cs, FPat p)
    _ -> mzero

{-
move :: Direction -> Context -> Maybe Context
move U (c:cs, e) = case c of
    FromModule id pr ne -> Just (cs, Module id $ reverse pr ++ e : ne)
    FromDef id    -> Just (cs, Def id e)
    FromLambda id -> Just (cs, Lambda id e)
    FromApp pr ne -> Just (cs, App $ reverse pr ++ e : ne)
move D (cs, e) = case e of
    Module id (e':es) -> Just (FromModule id [] es : cs, e')
    Def id e' -> Just (FromDef id : cs, e')
    Lambda id e' -> Just (FromLambda id : cs, e')
    App (e': es) -> Just (FromApp [] es : cs, e')
    _ -> Nothing
move R (c:cs, e) = case c of
    FromModule id pr (e' : ne) -> Just (FromModule id (e:pr) ne : cs, e')
    FromApp pr (e' : ne) -> Just (FromApp (e:pr) ne : cs, e')
    _ -> Nothing
move L (c:cs, e) = case c of
    FromModule id (e' : pr) ne -> Just (FromModule id pr (e:ne) : cs, e')
    FromApp (e' : pr) ne -> Just (FromApp pr (e:ne) : cs, e')
    _ -> Nothing
move _ _ = Nothing

append :: Direction -> Context -> Maybe Context
append L (c : cs, e) = case c of
    FromModule i pr ne -> Just (FromModule i pr (e:ne) : cs, Hole)
    FromApp pr ne      -> Just (FromApp pr (e:ne) : cs, Hole)
    _ -> Nothing
append R (c : cs, e) = case c of
    FromModule i pr ne -> Just (FromModule i (e:pr) ne : cs, Hole)
    FromApp pr ne      -> Just (FromApp (e:pr) ne : cs, Hole)
    _ -> Nothing
append _ _ = Nothing

delete :: Context -> Maybe Context
delete (c : cs, _) = case c of
    FromModule i pr (e:ne) -> Just (FromModule i pr ne : cs, e)
    FromModule i (e:pr) [] -> Just (FromModule i pr [] : cs, e)
    FromApp pr (e:ne) -> Just (FromApp pr ne : cs, e)
    FromApp (e:pr) [] -> Just (FromApp pr [] : cs, e)
    _ -> Nothing
delete _ = Nothing
-}
