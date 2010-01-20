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
import Expr
import Eval

type EIO =  IO

runEIO :: EIO a -> IO a
runEIO e = e
        
prContext :: (p -> [Doc] -> Doc) -> Context p-> EIO ()
prContext pr (tr, f) = do
    io $ putDoc $ maybe (text "error!!") (prTree pr) $ transform tr (Focus f)


defExpr :: EIO (Context Program)
defExpr = return
    ([], Node (PDef $ Module "test") 
            [ Node (PDef $ Def "id") 
                [Node (PExpr App)
                    [ Node (PExpr $ Lambda "x")
                        [ Node (PExpr $ Var "x") []
                        ]
                    , Hole
                    , Node (PExpr $ Var "y") []
                    , Hole
                    ]
                ]
            , Node (PDef $ Def "test") [Hole]
            ])

main :: IO ()
main = runEIO $ defExpr >>= loop

io = liftIO

reset :: EIO ()
reset = io $ do
    clearScreen
    setCursorPosition 0 0

loop :: Context Program -> EIO ()
loop c@(cs, e) = do
    reset
    prContext prProgram c
    io $ putStrLn ""
    x <- io getChar
    case x of
        'q' -> reset
        --'c' -> create c
        'e' -> next evalC c
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
        evalC (cs, f) = (,) cs `liftM` eval f
{-
create :: Context String -> EIO ()
create (cs, Hole) = do
    str <- io getLine
    loop (cs, Node str [Hole])
create ctx = loop ctx
-}
