module EditCommands where

import qualified Graphics.Vty as V

import Edit
import Moves 

-- | Updates the insert buffer without checking with the typechecker
insert :: Cmd ()
insert = isEdit ?-> 
         isAscii ' ' ~-> 
         getAscii >--> \c -> 
         getInput >--> \s -> 
         setInput (s ++ [c])

-- | Quits when q is pressed in control mode
quits :: Cmd ()
quits = isControl ?-> 
        isAscii 'q' ?-> 
        quit

-- | Trys to move in the tree
move :: Cmd ()
move = getKey   >--> \ke ->
       getFocus >--> \focus -> 
       case ke of
            V.KLeft  -> move' focus moveLeft
            V.KRight -> move' focus moveRight
            V.KUp    -> move' focus moveUp
            V.KDown  -> move' focus moveDown
            _        -> const stop
  where
    move' :: ExprCtx -> Move -> Cmd ()
    move' focus mv = case mv focus of
        Nothing  -> const stop
        Just foc -> setFocus foc


