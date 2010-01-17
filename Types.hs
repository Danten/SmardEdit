module Types where 

import Data.Map

data Expr
    = Module Id [Expr]
    | Def Id Expr
    | Lambda Id Expr
    | App [Expr]
    | Var Id
    | Hole | Focus Expr
    deriving (Eq, Show)

type Id = Integer

type Env = Map Id String

data Step
    = FromModule Id [Expr] [Expr]
    | FromDef Id
    | FromLambda Id
    | FromApp [Expr] [Expr]
    deriving (Eq, Show)


type Trace = [Step]
type Context = (Trace, Expr)

data Direction = L | R | U | D

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
