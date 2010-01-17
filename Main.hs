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
prContext (tr, e) = do
    env <- gets snd
    io $ putDoc $ prExpr env $ 
      foldr (flip (.)) id (map prStep tr) $ Focus e

focus :: Doc -> Doc
focus x = f lbracket <> x <> f rbracket
    where f = bold . red

prStep :: Step -> (Expr -> Expr)
prStep s hole = case s of
    FromModule id pr ne -> Module id $ reverse pr ++ hole:ne
    FromDef id -> Def id hole
    FromLambda id -> Lambda id hole
    FromApp pr ne -> App $ reverse pr ++ hole : ne


prKey :: String -> Doc
prKey = blue . bold . text

prId :: Env -> Id -> Doc
prId env id = case M.lookup id env of
    Nothing -> ondullred $ text "?ERROR?"
    Just i  -> green $ text i

prExpr :: Env -> Expr -> Doc
prExpr en exp = prExpr' 0 en exp
    where 
        prExpr' bb en exp = case exp of
            Module id es -> par bb $ prKey "module" <+> prId en id <> line
                <> indent 2 (vsep . intersperse empty $ map (prExpr' 0 en) es)
            Def id e -> par bb $ prKey "define" <+> prId en id <> line 
                <> indent 2 (prExpr' 0 en e)
            Lambda id e -> par bb $ prKey "lambda" <+> prId en id <> line 
                <> indent 2 (prExpr' 0 en e)
            App es -> par bb $ hsep $ map (prExpr' 0 en) es
            Var id -> prId en id
            Focus e -> focus $ prExpr' 1 en e
            _ -> cyan $ text "<?>"
        par 0 = parens
        par _ = id 


defExpr :: EIO Context
defExpr = do 
    [mid, id, x] <- mapM newId ["test", "id", "x"]
    return (FromModule mid [Def id $ Lambda x (Var x)] [] : [], Hole)

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
        'c' -> create c
        'a' -> loop . fromJust $ append R c `mplus` return c
        'i' -> loop . fromJust $ append L c `mplus` return c
        'd' -> loop . fromJust $ delete c `mplus` return c
        'h' -> mov L c
        'j' -> mov D c
        'k' -> mov U c
        'l' -> mov R c
        'e' -> loop (cs, Hole)
        'v' -> createVar c 
        _ -> loop c


mov d c = loop . fromJust $ move d c `mplus` return c

newId :: String -> EIO Id
newId s = do
    (id, env) <- get
    put $ (id + 1, M.insert id s env)
    return id

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
