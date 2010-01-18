module Main where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Data.List (intersperse)
import qualified Data.Map as M
import Data.Maybe

import System.IO
import System.Console.ANSI

import Text.PrettyPrint.ANSI.Leijen

import Types

type EIO = StateT (Id, Env) IO

runEIO :: EIO a -> IO a
runEIO e = evalStateT e (0, M.empty)

        
prContext :: Context -> EIO ()
prContext (tr, f) = do
    env <- gets snd
    io $ putDoc $ maybe (text "error!!") (prFocus env) $ transform tr (addFocus f)

addFocus :: Focus -> Focus
addFocus f = case f of
    FDef  x -> FDef  $ DFocus x
    FExpr x -> FExpr $ EFocus x
    FPat  x -> FPat  $ PFocus x

transform :: Trace -> Focus -> Maybe Focus
transform [] f = return f
transform (c : cs) f = transform cs =<< case c of
    FromModule id pr ne -> (FDef . Module id . bet pr ne) `liftM` getDef f
    FromDef id -> (FDef . ValDef id) `liftM` getExpr f
    FromLambda id -> (FExpr . Lambda id) `liftM` getExpr f
    FromCaseScrut pats -> (FExpr . flip Case pats) `liftM` getExpr f
    FromCasePat scrut pr e ne
        -> (FExpr . Case scrut . bet pr ne . flip (,) e) `liftM` getPat f
    FromCaseExpr scrut pr p ne
        -> (FExpr . Case scrut . bet pr ne . (,) p) `liftM` getExpr f
    FromApp pr ne -> (FExpr . App . bet pr ne) `liftM` getExpr f
    FromPApp pr ne -> (FPat . PApp . bet pr ne) `liftM` getPat f
     

prFocus :: Env -> Focus -> Doc
prFocus en f = case f of
    FDef d  -> prDef  en d
    FExpr e -> prExpr en e
    FPat p  -> prPat  en p

focus :: Doc -> Doc
focus x = f lbracket <> x <> f rbracket
    where f = bold . red


prKey :: String -> Doc
prKey = blue . bold . text

prId :: Env -> Id -> Doc
prId env id = case M.lookup id env of
    Nothing -> ondullred $ text "?ERROR?"
    Just i  -> green $ text i

prDef :: Env -> Def -> Doc
prDef en def = case def of
    Module id es -> parens $ prKey "module" <+> prId en id <> line
        <> indent 2 (vsep . intersperse empty $ map (prDef en) es)
    ValDef id e -> parens $ prKey "define" <+> prId en id <> line 
        <> indent 2 (prExpr en e)
    DFocus def -> focus $ prDef en def
    _ -> prHole

prExpr :: Env -> Expr -> Doc
prExpr en exp = case exp of
    Lambda id e -> parens $ prKey "lambda" <+> prId en id <> line 
        <> indent 2 (prExpr en e)
    App es -> parens $ hsep $ map (prExpr en) es
    Var id -> prId en id
    EFocus e -> focus $ prExpr en e
    _ -> prHole

prPat :: Env -> Pat -> Doc
prPat en pat = case pat of
    PApp ps -> parens $ hsep $ map (prPat en) ps
    PVar id -> prId en id
    PFocus p-> focus $ prPat en p
    _ -> prHole

prHole :: Doc
prHole = cyan $ text "<?>"

defExpr :: EIO Context
defExpr = do 
    [mid, id, x] <- mapM newId ["test", "id", "x"]
    return (FromModule mid [ValDef id $ Lambda x (Var x)] [] : [], FDef DHole)

main :: IO ()
main = runEIO $ defExpr >>= loop

io = liftIO

reset :: EIO ()
reset = io $ do
    clearScreen
    setCursorPosition 0 0

loop :: Context -> EIO ()
loop c@(cs, e) = do
    reset
    prContext c
    io $ putStrLn ""
    x <- io getChar
    case x of
        'q' -> reset
        --'c' -> create c
        --'a' -> loop . fromJust $ append R c `mplus` return c
        --'i' -> loop . fromJust $ append L c `mplus` return c
        --'d' -> loop . fromJust $ delete c `mplus` return c
        'h' -> next moveLeft c
        'j' -> next moveDown c
        'k' -> next moveUp c
        'l' -> next moveRight c
        --'e' -> loop (cs, Hole)
        --'v' -> createVar c 
        _ -> loop c
    where
        next f c = loop $ maybe c id (f c)

--mov d c = loop . fromJust $ move d c `mplus` return c

newId :: String -> EIO Id
newId s = do
    (id, env) <- get
    put $ (id + 1, M.insert id s env)
    return id
{-
create :: Context -> EIO ()
create (c, Hole) = do
    x <- io getChar
    let getId = io getLine >>= newId
    case x of
        'm' -> getId >>= \i -> loop (FromModule i [] [] : c, Hole)
        'd' -> getId >>= \i -> loop (FromDef i : c, Hole)
        'l' -> getId >>= \i -> loop (FromLambda i : c, Hole)
        'a' -> loop (FromApp [] [] : c, Hole)
        _ -> loop (c, Hole)
create c = loop c

createVar :: Context -> EIO ()
createVar (c, Hole) = do
    id <- io getLine >>= newId
    loop (c, Var id)
createVar c = loop c

-}
