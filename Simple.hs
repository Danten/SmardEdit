{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Simple where

import Data.Set (Set)
import qualified Data.Set as S

import Control.Monad
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Reader

type Ident = String

data Expr
    = ELit Lit --EInt  Integer
    | EHole
    | EFocus Expr
    | EVar Ident
    | EBin Ident Expr Expr
    | EApp Expr Expr
    | EAbs Ident Expr
    deriving (Eq, Ord, Show)

data Lit
    = LInt Int
    | LBool Bool
    deriving (Eq, Ord, Show)

data Type
    = TInt
    | TBool
    | TVar Ident
    | TFun Type Type
    deriving (Eq, Ord)

instance Show Type where
    show TInt     = "Int"
    show TBool    = "Bool"
    show (TVar t) = t
    show (TFun a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"

type Vars = []
type Env = [(Ident, Type)]

newtype TCm a = TCm { unTCm :: ReaderT Env (State (Vars Ident)) a }
    deriving (Monad, MonadState (Vars Ident), MonadReader Env)

type Constraint = (Type, Type)
type Constraints = Set Constraint

{-
class (Show t, Eq t, Ord t) => TCV t where

instance TCV Char where

instance TCV t => TCV [t] where


-}

runTC :: Env -> Vars Ident -> TCm a -> a
runTC env vars = flip evalState vars . flip runReaderT env . unTCm

infVars :: Enum t => t -> t -> [[t]]
infVars low high = [1..] >>= flip replicateM [low..high]

vars = infVars 'a' 'z'

mkConstraint :: Type -> Type -> Constraint
mkConstraint = (,)

newVar :: TCm Type
newVar = do
    x:xs <- get
    put xs
    return (TVar x)

cg :: Expr -> TCm (Type , Constraints )
cg e = case e of
    EFocus e' -> cg e'
    EHole -> newVar >>= \(TVar v) -> return (TVar $ "h." ++ v, S.empty)
    ELit (LInt _) -> return (TInt, S.empty)
    ELit (LBool _) -> return (TBool, S.empty)
    EAbs iden exp -> do
        x <- newVar
        (typ, c) <- local ((iden,x):) $ cg exp
        return (TFun x typ, c)  
    EVar t -> do 
        env <- ask
        case lookup t env of
            Nothing    -> error $ "I don't know..." ++ show t
            Just  typ  -> return (typ, S.empty)
    EBin t e1 e2 -> cg $ EApp (EApp (EVar t) e1) e2
    {-
        (t1, c1) <- cg e1
        (t2, c2) <- cg e2
        return (TInt, c1 `S.union` c2 `S.union` 
                      S.fromList [ mkConstraint t1 TInt
                                 , mkConstraint t2 TInt]
               )
     -}
    EApp e1 e2 -> do
        (t1, c1) <- cg e1
        (t2, c2) <- cg e2
        x <- newVar
        return (x, c1 `S.union` c2 `S.union` S.singleton (t1,TFun t2 x))


unify :: Constraints -> Env --Type t -> Type t -- [(t, Type t)] ~= Subst t
unify c | S.null c  = []
        | otherwise = case (t1, t2) of
            (TInt, TInt) -> unify c'
            (TBool, TBool) -> unify c'
            --(TVar a, TInt) -> (a, TInt) : unify (substConst a TInt c')
            --(TInt, TVar a) -> (a, TInt) : unify (substConst a TInt c')
            (TFun ta tb, TFun ta' tb') -> unify (S.insert (ta, ta') 
                                                  (S.insert (tb, tb') c'))
--            (TFun ta tb, TFun ta' tb') -> let s1 = unify (S.singleton (ta, ta'))
--                                              s2 = unify (S.singleton (subst s1 tb, subst s1 tb'))
--                                             in s1 ++ s2 ++ unify c' --S.unions [S.fromList s1,S.fromList s2,c]
                                                  
            (TVar x, t) | x `S.member` freeVar t -> error $ "recurrences of " ++ show x ++ " in " ++ show t
                        | otherwise              -> unify (substConst x t c') ++  [(x, t)]
            (t, TVar x) | x `S.member` freeVar t -> error "reoccurences stavs sa?"
                        | otherwise              -> unify (substConst x t c') ++  [(x, t)]                        
            --(t, TVar x) -> unify (substConst x t c') ++ [(x, t)]
            mis -> error $ show mis
  where
    ((t1, t2), c') = S.deleteFindMin c
    substConst f rep set = S.map (\(x,y) -> (substType f rep x, substType f rep y)) set
    freeVar :: Type -> Set Ident
    freeVar typ = case typ of
        TVar x -> S.singleton x
        TFun x y -> freeVar x `S.union` freeVar y
        _        -> S.empty

substType :: Ident -> Type -> Type -> Type
substType x tobe is = case is of
    TVar y | x == y -> tobe
    TFun t1 t2 -> TFun (substType x tobe t1) (substType x tobe t2)
    _ -> is

subst :: Env -> Type -> Type
subst [] = id
subst ((x,t) :es) = substType x t . subst es

app = EApp

plus = EBin "+"
eif p a b = EVar "if" `app` p `app` a `app` b --EApp (EApp (EApp (EVar "if") p) a) b 

int = ELit . LInt

hole = EHole

ex2 = ((EApp (EVar "f") $ int 2 `plus` (EVar "y" `plus` EVar "x")) `plus` EVar "x")

ex3 = fun ["f", "g", "x"] (EApp (EVar "f") (EApp (EVar "g") (EVar "x")))


fun :: [String] -> Expr -> String
fun arg exp = let (c,env) = runTC (("+", TFun TInt (TFun TInt TInt))
                                  :("if", TFun TBool (TFun (TVar "1") (TFun (TVar "1") (TVar "1"))))
                                  : (zip arg (map (\x -> TVar ("t." ++ x)) arg))) vars $ cg exp 
               in concatMap' " -> " (show . subst (unify env)) (map (\x -> TVar ("t." ++ x)) arg) ++ " -> " ++ show (subst (unify env) c)
  where
    concatMap' _ _ []     = ""
    concatMap' c f (x:[]) = f x
    concatMap' c f (x:xs) = f x ++ c ++ concatMap' c f xs
    
mun arg exp = let (c,env) = runTC (("+", TFun TInt (TFun TInt TInt))
                                  :("if", TFun TBool (TFun (TVar "1") (TFun (TVar "1") (TVar "1"))))
                                  : (zip arg (map (\x -> TVar ("t." ++ x)) arg))) vars $ cg exp 
               in (env,(unify env))