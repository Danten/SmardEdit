module Edit where

import Control.Monad
import Data.List
import Graphics.Vty

import Simple
import Moves

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

data Mode
    = Control
    | Input String
    deriving (Eq, Show)

data CmdRes t
    = Quit
    | Keep String
    | Change t

instance Monad CmdRes where
    return = Change

    (Change t) >>= f = f t
    Keep s >>= _ = Keep s
    Quit   >>= _ = Quit

instance MonadPlus CmdRes where
  mzero = Keep "MZero"
  c@(Change _) `mplus` _ = c
  _ `mplus` c = c

type Cmd = EditState -> CmdRes EditState

scope :: Env -> Env -> Env
scope = (++)

conScope :: Ident -> Env -> Env -> Type -> Env
conScope iden e1 e2 t = 
                    filter (\(name, typ) -> iden `isPrefixOf` name
                            && lessOrEqual t typ) $ scope e1 e2
  where
    lessOrEqual typ1 typ2 | typ1 == typ2 = True
    lessOrEqual typ1 (TFun _ typ2')  = lessOrEqual typ1 typ2'
    lessOrEqual typ1 (TVar _) = True
    lessOrEqual _ _ = False 

controlActions :: EditState -> [(Key, String, Cmd)]
controlActions editState = 
    [ (KLeft , "left" , move moveLeft)
    , (KRight, "right", move moveRight)
    , (KUp   , "up"   , move moveUp)
    , (KDown , "down" , move moveDown)
    ] ++ case mode editState of
    Input _ -> [ (KEsc, "escape", esc)
               ]
    Control -> case focus editState of
        (ctx, expr) -> case expr of 
            _ -> [ (KASCII 'd', "delete", deleteC) 
                 , (KASCII 'e', "edit"  , edit)
                 , (KASCII 'q', "quit"  , const Quit)
                 ]

-- | x >-> y, do x if succeed do y
(>->) :: Cmd -> Cmd -> Cmd
x >-> y = \e -> x e >>= y

-- | x >?> y, do x if fail do y
(>?>) :: Cmd -> Cmd -> Cmd
x >?> y = \e -> x e `mplus` y e

move :: Move -> Cmd
move move edit = case move $ focus edit of
    Nothing  -> Keep "couldn't move"
    Just foc -> Change edit { focus = foc }

replace :: Expr -> Cmd
replace exp edit = case focus edit of
    (ctx, _) -> return edit { focus = (ctx, exp) }

deleteC :: Cmd
deleteC = replace EHole

-- | checkExpr succeeds if the expression fulfills the predicates, otherwise fail
checkExpr :: (Expr -> Bool) -> Cmd
checkExpr p edit = case focus edit of
    (_, e) -> case p e of
        True  -> return edit
        False -> mzero

isAtHole :: Cmd
isAtHole = checkExpr (== EHole)

insertC :: Cmd 
insertC = isAtHole >-> \edit -> return edit
    { mode = Input "" }

edit :: Cmd
edit = deleteC >-> insertC

insertChar :: Char -> Cmd
insertChar x edit = case mode edit of
    Input str -> return edit
        { mode = Input $ str ++ [x]}
    _ -> mzero

esc :: Cmd
esc edit = case mode edit of
    Input _ -> return edit { mode = Control }
    _ -> mzero