module Commands
  ( Commands -- :: *
  , register -- :: Cmd -> Event -> Commands -> Commands
  , registerMany -- :: [(Cmd, Event)] -> Commands -> Commands
  , fromList     -- :: 
  , run  -- :: EditState -> Event -> Commands -> Maybe (CmdRes EditState)
  ) where

import Data.Function
import Data.List

import Edit2


type Commands = [(Prec, Cmd)]
type Prec     = Int

empty :: Commands
empty = []

register :: Prec -> Cmd -> Commands -> Commands
register p c cs = insertBy (compare `on` fst) (p, c) cs

registerMany :: [(Prec, Cmd)] -> Commands -> Commands
registerMany = foldr (uncurry register) 

fromList :: [(Prec, Cmd)] -> Commands
fromList = sortBy (compare `on` fst)

run :: EditState -> Event -> Commands -> CmdRes EditState
run es e [] = NoChange
run es e ((_,c):cs) = case c (es, e) of
    NoChange -> run es e cs
    cres     -> cres
