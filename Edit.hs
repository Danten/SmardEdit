module Edit
  ( Event(..)
  , Mode(..)
  , EditState(..)
  , CmdRes(..)
  , Cmd
  , defaultEditState
  ---- The language ---
  -- Combinators
  , (?->)
  , (~->)
  , (>->)
  , (>-->)
  , stop
  -- Modifiers
  , edit
  , control
  , quit
  , setInput
  , setFocus
  -- Questions
  , isEdit
  , isControl
  , isHole
  , isAscii
  -- Getters
  , getFocus
  , getAscii
  , getInput
  , getKey
  ) where

import Control.Monad
import Data.Maybe

import qualified Graphics.Vty as V

import Simple
import Moves

infixr 5 ?->

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
        , mode       = Edit "Hejsan" -- Control
        }

data CmdRes t
    = Quit
    | Change t
    | NoChange
--    | Failure t
  deriving Show

    {-
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
-}
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

change x = Change (x, ())
same   x (e,_) = Change (e, x)

type CmdInput = (EditState, Event) 
--type Cmd = Cmd () -- CmdInput -> CmdRes EditState
type Cmd t = CmdInput -> CmdRes (EditState, t)
type Que = CmdInput -> Bool

--- Modifiers ---
-- | Enters Edit mode
edit :: Cmd ()
edit = setMode (Edit {inputStr = ""})

-- | Enters control mode
control :: Cmd () 
control = setMode Control

setMode :: Mode -> Cmd ()
setMode md (es, ev) = change $ es {mode = md}

setInput :: String -> Cmd ()
setInput str = setMode (Edit {inputStr = str})

setFocus :: ExprCtx -> Cmd ()
setFocus ec (es, ev) = change $ es { focus = ec } 

-- | Quits the application
quit :: Cmd ()
quit = const Quit

--- Getters ---
-- | Gives the current focus
getFocus :: Cmd ExprCtx
getFocus e@(es,_) = getFocus' (\c -> same c e) e

getFocus' :: (ExprCtx -> t) -> CmdInput -> t
getFocus' f (es, _) = f (focus es)

getAscii' :: (Char -> t) -> t -> CmdInput -> t
getAscii' f g (_, ev) = case ev of
    KeyEvent (V.KASCII x) -> f x
    _                     -> g

getAscii :: Cmd Char
getAscii e@(es,_) = getAscii' (\c -> same c e) stop e

getInput' :: (String -> t) -> CmdInput -> t
getInput' f (es, _) = f $ inputStr (mode es)

getInput :: Cmd String
getInput e@(es,_) = getInput' (\s -> same s e) e


getKey :: Cmd V.Key
getKey e@(_,KeyEvent ke) = same ke e
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
isHole = getFocus' (\(_,e) -> case e of
    EHole -> True
    _     -> False
    )

isAscii :: Char -> Que
isAscii c = getAscii' ((==)c) False


--- Combinators ---
-- | p ?-> g, do g if predicate p is true
(?->) :: Que -> Cmd t -> Cmd t
(?->) p g e = case p e of
    True  -> g e
    False -> stop

(~->) :: Que -> Cmd t -> Cmd t
(~->) p g e = case not (p e) of
    True  -> g e
    False -> stop

{-
-- | f >-> g, if f succeds do g
(>->) :: Cmd -> Cmd -> Cmd
f >-> g = \e@(_,ev) -> case f e of
    Change x -> g (x,ev)
    x        -> x
-}

f >-> g = f >--> const g

(>-->) :: Cmd s -> (s -> Cmd t) -> Cmd t
f >--> g = \e@(_,ev) -> case f e of
    Change (x,y) -> g y (x, ev)
    NoChange -> NoChange
    Quit     -> Quit

-- (>>=) 

--insert = isAscii ?-> getAscii' >-> undefined 

--- Functions (move me to another file, pretty please) --- 



--- Tests ---

--- Edit state should not change
--- e == ((const False) ?-> s) e 
