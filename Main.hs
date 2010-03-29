module Main where

import Simple
import Moves
import Edit
import Pretty

import System.IO
--import System.Console.ANSI
import System.Posix hiding (Quit)
import Graphics.Vty

{-
main = do
  vty <- mkVty
  e   <- next_event vty
  print e
-}


main :: IO ()
main = do
  vty <- mkVty
  loop vty defaultState
--reset >> setTitle "Awsome editor!" >> loop defaultState
  where
    defaultState = EditState
        { prevDefs   = []
        , nextDefs   = []
        , currentDef = ("main", [], TInt)
        , topGamma   = [("main", TInt)]
        , localGamma = []
        , focus      = ([], int 3 `plus` int 43 `plus` int 67)
        , mode       = Control
        }

{-
    Take a look at initTermInput instead of passing around vty

-}

{-
reset :: IO ()
reset = do
    clearScreen
    setCursorPosition 0 0
-}

next :: Vty -> Move -> EditState -> IO ()
next vty move edit = case move $ focus edit of
    Just edit' -> loop vty (edit { focus = edit' })
    Nothing    -> loop vty edit

loop :: Vty -> EditState -> IO ()
loop vty es =  do
    -- reset
    --xputStrLn $ "[" ++ show (mode es) ++ "]"
    -- putStrLn ""
    update vty $ pic_for_image $ string def_attr $ show $ prExprCtx (focus es)

    evt <- next_event vty
    refresh vty  -- need to set update first...
    case evt of
        EvKey key modifes -> case filter (\(c,_,_) -> key == c) (controlActions es) of
                [] -> loop vty es
                ((_,_, cmd):_) -> case cmd es of
                       Quit -> shutdown vty -- return ()
                       Keep s -> putStrLn s >> loop vty es
                       Change es' -> loop vty es'
{-        
    case filter (\(c,_,_) -> x == c) (controlActions es) of
        [] -> loop es
        ((_,_, cmd):_) -> case cmd es of
            Quit -> return ()
            Keep s -> putStrLn s >> loop vty es
            Change es' -> loop vty es'
-}
{-    case filter (\(c,_,_) -> x == c) (controlActions c) of
        []    -> case x of

            '\^[' -> do getChar   -- ugly x 100 hack for arrow keys, won't work everywhere, sorry 
                        x <- getChar
                        case x of
                            'B' -> next moveDown c
                            'D' -> next moveLeft c
                            'C' -> next moveRight c
                            'A' -> next moveUp c
                            x   -> putStrLn $ "new char, cool: " ++ [x]
            _   -> case mode c of
                Control -> case x of
                    'q' -> return ()
                    _   -> loop c
                Input _ -> loop c
        ((_,_, e):_) -> case e c of
            Nothing -> putStrLn "looser!" >> loop c
            Just x  -> loop x
-}