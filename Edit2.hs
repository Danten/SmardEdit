module Edit2 
  ( Event(..)
  , Mode(..)
  , EditState(..)
  , CmdRes(..)
  ---- The language ---
  -- Combinators
  , (?->)
  , (>->)
  -- Modifiers
  , edit
  , control
  , quit
  -- Questions
  , isEdit
  , isControl
  , isHole
  -- Getters
  , getFocus
  ) where

import Control.Monad
import Data.List
import Data.Maybe

import qualified Graphics.Vty as V

import Simple
import Moves

data Event = KeyEvent V.Key
  deriving Eq
  
data Mode
  = Control
  | Edit 
    { inputStr :: String }
 deriving (Eq, Show)


data Def = Def Ident [Ident] Expr Type
    deriving (Eq, Show)

data EditState = EditState
    { prevDefs   :: [Def]
    , nextDefs   :: [Def]
    , currentDef :: (Ident, [Ident], Type)
    , topGamma   :: Env
    , localGamma :: Env
    , focus      :: ExprCtx
    , mode       :: Mode
    } deriving (Eq, Show)

-- defaultEditState = EditState [] [] (

defaultEditState = EditState
        { prevDefs   = []
        , nextDefs   = []
        , currentDef = ("main", [], TInt)
        , topGamma   = [("main", TInt)]
        , localGamma = []
        , focus      = ([], int 3 `plus` int 43 `plus` int 67)
        , mode       = Control
        }

data CmdRes t
    = Quit
    | Change t
    | NoChange
--    | Failure t
  deriving Show

instance Monad CmdRes where
    return           = Change
    (Change t) >>= f =  f t 
    
    {- case f t of
        Failure _ -> Failure t
        x         -> x
    -}
    Quit       >>= _ = Quit
    NoChange   >>= _ = NoChange
--    Failure t  >>= f = f t

{-
instance Monad CmdRes where
    return = Change

    (Change t) >>= f = f t
    Keep s >>= _ = Keep s
    Quit   >>= _ = Quit
    Failure t >>= f  = Failure t 
-}
{-
instance MonadPlus CmdRes where
  mzero = Keep "MZero"
  c@(Change _) `mplus` _ = c
  _ `mplus` c = c

stop :: CmdRes t
stop = mzero
stopBecause = 
-}

stop :: CmdRes t
stop = NoChange

change = Change

type CmdInput = (EditState, Event) 
type Cmd = CmdInput -> CmdRes EditState
type Cmd' t = CmdInput -> CmdRes (EditState, t)
type Que = CmdInput -> Bool

--- Modifiers ---
-- | Enters Edit mode
edit :: Cmd
edit = setMode (Edit {inputStr = ""})

-- | Enters control mode
control :: Cmd 
control = setMode Control

setMode :: Mode -> Cmd
setMode md (es, ev) = change $ es {mode = md}

-- | Quits the application
quit :: Cmd
quit = const Quit

--- Getters ---
-- | Gives the current focus
getFocus :: (ExprCtx -> t) -> CmdInput -> t
getFocus f (es, _) = f (focus es)

getAscii :: (Char -> t) -> t -> CmdInput -> t
getAscii f g (_, ev) = case ev of
    KeyEvent (V.KASCII x) -> f x
    _                     -> g

getAscii' :: Cmd' Char
getAscii' e@(es,_) = getAscii (\c -> return (es ,c)) stop e

getInput :: (String -> t) -> CmdInput -> t
getInput f (es, _) = f $ inputStr (mode es)

--- Askers ---

-- | Is the edit mode active
isEdit :: Que
isEdit (e,_) = case mode e of
    Edit _ -> True
    _      -> False

isControl :: Que
isControl (e,_) = case mode e of
    Control -> True
    _       -> False

-- | Is the foucs at a hole
isHole :: Que
isHole = getFocus (\(_,e) -> case e of
    EHole -> True
    _     -> False
    )

isAscii :: Que
isAscii = getAscii (const True) False


--- Combinators ---
-- | p ?-> g, do g if predicate p is true
(?->) :: Que -> Cmd -> Cmd
(?->) p g e = case p e of
    True  -> g e
    False -> stop

-- | f >-> g, if f succeds do g
(>->) :: Cmd -> Cmd -> Cmd
f >-> g = \e@(_,ev) -> case f e of
    Change x -> g (x,ev)
    x        -> x

{-
(>=>) :: Cmd' s -> (s -> Cmd) -> Cmd
f >=> g = 
-}
-- (>>=) 

--insert = isAscii ?-> getAscii' >-> undefined 

--- Functions (move me to another file, pretty please) --- 



--- Tests ---

--- Edit state should not change
--- e == ((const False) ?-> s) e 
