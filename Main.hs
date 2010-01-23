{-# LANGUAGE PatternGuards #-}
module Main where


import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader

import Data.List (intersperse, isPrefixOf)
import qualified Data.Map as M
import Data.Maybe

import System.IO
import System.Console.ANSI

import Text.PrettyPrint.ANSI.Leijen

import Types
import Expr
import Eval

type EIO =  IO

data Mode
    = Control
    | Insert String
    deriving (Eq, Show)

runEIO :: EIO a -> IO a
runEIO e = e
        
prContext :: (p -> [Doc] -> Doc) -> Context p-> EIO ()
prContext pr (tr, f) = do
    io $ putDoc $ maybe (text "error!!") (prTree pr) $ transform tr (Focus f)


defExpr :: EIO (Context Program)
defExpr = return
    ([], Node (PDef Module) 
            [ Node (PBind $ Single "test") []
            , Node (PDef Def) 
                [ Node (PBind $ Single "id") []
                , Node (PType TFun)
                    [ Node (PType $ TVar "int") []
                    , Node (PType $ TVar "int") []
                    ]
                , Node (PExpr App)
                    [ Node (PExpr Lambda)
                        [ Node (PBind $ Single "x") []
                        , Node (PExpr $ Var "x") []
                        ]
                    , Hole
                    , Node (PExpr $ Var "y") []
                    , Hole
                    ]
                ]
            , Node (PDef Def) 
                [ Node (PBind $ Single "test") []
                , Hole
                , Hole
                ]
            ])

main :: IO ()
main = runEIO $ defExpr >>= loop Control

io = liftIO

reset :: EIO ()
reset = io $ do
    clearScreen
    setCursorPosition 0 0

loop :: Mode -> Context Program -> EIO ()
loop Control c@(cs, e) = do
    reset
    io $ putStrLn "[Control]"
    io $ putStrLn ""
    prContext prProgram c
    io $ putStrLn ""
    x <- io getChar
    case x of
        'q' -> reset
        --'e' -> next evalC c
        --'a' -> next appendRight c
        --'A' -> next appendLeft c
        'd' -> next myDelete c
        'c' -> loop (Insert "") $ maybe c id (myDelete c)
        'h' -> next moveLeft c
        'j' -> next moveDown c
        'k' -> next moveUp c
        'l' -> next moveRight c
        'i' -> loop (Insert "") c
        'p' -> do 
            print $ transform cs e
            getChar
            loop Control c
        _ -> case currentMFocus c of 
            MDef -> case x of
                _ -> loop Control c
            MExpr -> case x of
                '(' -> case e of
                    Hole -> loop (Insert "") (InvNode (PExpr App) [] [] : cs, Hole)
                    _ -> loop (Insert "") (InvNode (PExpr App) [e] [] : cs, Hole)
                _ -> loop Control c
            MType -> case x of
                _ -> loop Control c
            MBind -> case x of
                _ -> loop Control c
            None -> case x of
                _ -> loop Control c
    where
        next f c = loop Control $ maybe c id (f c)
        -- evalC (cs, f) = (,) cs `liftM` eval f
loop (Insert pr) c@(cs, e) = do
    reset
    io $ putStrLn $ "[Insert] " ++ pr
    io $ putStrLn ""
    prContext prProgram c
    io $ putStrLn ""
    x <- io getChar
    case x of
        ' ' -> parseAppendArg pr c
        '\DLE' -> parseKeyword pr c
        '\n' -> createVar False pr c
        '\t' -> createVar True pr c
        '\ESC' -> loop Control c
        '(' | null pr -> case e of
            Hole -> loop (Insert "") (InvNode (PExpr App) [] [] : cs, Hole)
            _ -> loop (Insert "") (InvNode (PExpr App) [e] [] : cs, Hole)
        _ -> loop (Insert $ pr ++ [x]) c

myDelete :: Context Program -> Maybe (Context Program)
myDelete (c:cs, Hole) | snd (isFlexible c) && isEmpty c = return (cs, Hole)
                      | snd (isFlexible c) =  delete (c:cs, Hole)
                      | otherwise    = mzero
    where isEmpty (InvNode _ pr ne) = null pr && null ne
myDelete (cs, _) = return (cs, Hole)
    
    

parseAppendArg :: String -> Context Program -> EIO ()
parseAppendArg id (InvNode (PExpr App) pr ne:cs, Hole)
    = loop (Insert "") (InvNode (PExpr App) (Node (PExpr $ Var id) [] : pr) ne : cs, Hole)
parseAppendArg id (InvNode (PBind Multiple) pr ne:cs, Hole)
    = loop (Insert "") (InvNode (PBind Multiple) (Node (PBind $ Single id) [] : pr) ne : cs, Hole)
parseAppendArg _ c = loop Control c

parseKeyword :: String -> Context Program -> EIO ()
parseKeyword id c@(cs, Hole) = case currentMFocus c of
    MDef -> case () of
        _ | id `isPrefixOf` "module" -> 
            loop (Insert "") (InvNode (PDef Module) [] [Hole] : cs, Hole)
          | id `isPrefixOf` "def" ->
            loop (Insert "") (InvNode (PDef Def) [] [Hole, Hole] : cs, Hole)
          | otherwise -> def
    MType -> case () of
        _ | id == "->" -> 
            loop Control (InvNode (PType TFun) [] [] : cs, Hole)
          | otherwise -> def
    MExpr -> case () of
        _ | id `isPrefixOf` "lambda" ->
            loop (Insert "") (InvNode (PExpr Lambda) [] [Hole] : cs, Hole)
          | otherwise -> def
    MBind -> def
    None -> def
    where
        def = loop Control c
parseKeyword _ c = loop Control c


createVar :: Bool -> String -> Context Program -> EIO ()
createVar b id (cs, t) = case t of
    Hole -> case currentMFocus (cs, t) of
        MDef -> def
        MBind -> doo $ cs' $ PBind . Single
        MType -> doo $ cs' $ PType . TVar
        MExpr -> doo $ cs' $ PExpr . Var
    _ -> def
    where
        cs' n = (cs, Node (n id) []) 
        def = loop Control (cs, t)
        doo cs' = case b of  
            True  -> loop (Insert "") $ maybe cs' (\x -> x) (moveRight cs')
            False -> loop Control cs'
{-
create :: Context String -> EIO ()
create (cs, Hole) = do
    str <- io getLine
    loop (cs, Node str [Hole])
create ctx = loop ctx
-}
