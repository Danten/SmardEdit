module Main where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader

import Data.List (intersperse)
import qualified Data.Map as M
import Data.Maybe

import System.IO
import System.Console.ANSI

import Text.PrettyPrint.ANSI.Leijen

import Types

type EIO =  IO

runEIO :: EIO a -> IO a
runEIO e = e

        
prContext :: (p -> [Doc] -> Doc) -> Context p-> EIO ()
prContext pr (tr, f) = do
    io $ putDoc $ maybe (text "error!!") (prTree pr) $ transform tr (Focus f)

focus :: Doc -> Doc
focus x = f lbracket <> x <> f rbracket
    where f = bold . red

prTree :: (p -> [Doc] -> Doc) -> Tree p -> Doc
prTree pr tree = case tree of
    Hole -> prHole
    Focus tree' -> focus $ prTree pr tree'
    Node p ps -> pr p (map (prTree pr) ps)

prKey :: String -> Doc
prKey = blue . bold . text

prHole :: Doc
prHole = cyan $ text "<?>"

defExpr :: EIO (Context String)
defExpr = return
    ([], Node "title" [Node "section 1" [Hole], Node "section 2" [Hole, Hole]])

main :: IO ()
main = runEIO $ defExpr >>= loop

io = liftIO

reset :: EIO ()
reset = io $ do
    clearScreen
    setCursorPosition 0 0

loop :: Context String -> EIO ()
loop c@(cs, e) = do
    reset
    prContext prS c
    io $ putStrLn ""
    x <- io getChar
    case x of
        'q' -> reset
        'c' -> create c
        'a' -> next appendRight c
        'i' -> next appendLeft c
        'd' -> next delete c
        'h' -> next moveLeft c
        'j' -> next moveDown c
        'k' -> next moveUp c
        'l' -> next moveRight c
        _ -> loop c
    where
        next f c = loop $ maybe c id (f c)
        prS s cs = prKey "+" <+> parens (text s) 
            <> if null cs then empty else linebreak <> indent 4 (vsep cs)

create :: Context String -> EIO ()
create (cs, Hole) = do
    str <- io getLine
    loop (cs, Node str [Hole])
create ctx = loop ctx
