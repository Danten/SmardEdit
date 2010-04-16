module UI where

import Simple
import Moves
import qualified Edit2 as E
import qualified Commands as C
import Pretty

import Control.Applicative
import System.IO
--import System.Console.ANSI
import System.Posix hiding (Quit)
import Graphics.Vty

import Graphics.Vty.Widgets.All

type View = E.EditState -> Widget

defAttr = def_attr

mainView :: View
mainView es = simpleText defAttr (show $ prExprCtx (E.focus es))

envView :: View
envView e = simpleText defAttr "env"

inputView :: View
inputView e = simpleText defAttr "input"

editView, controlView :: View
editView = bottomPadded . mainView <-|-> inputView <-|-> envView

controlView = (<-->) <$> bottomPadded . mainView <*> envView

infixr 1 <+|+>
infixr 1 <-|->
(<+|+>), (<-|->) :: View -> View -> View
f <+|+> g = \ x -> f x <++> g x
f <-|-> g = \ x -> f x <--> g x


rend :: Vty -> Widget -> IO Event
rend vty wid = do
    (img, _) <- mkImage vty wid
    update vty $ pic_for_image img
    next_event vty

eventloop :: Vty -> E.EditState -> C.Commands-> IO ()
eventloop vty estate cs = do
        evt <- rend vty (widget estate)
        case evt of
            EvKey ke mods -> case C.run estate (E.KeyEvent ke) cs of
                E.Quit     -> return ()
                E.NoChange -> eventloop vty estate cs 
                E.Change t -> eventloop vty t cs
            _ -> eventloop vty estate cs
  where
    widget = case E.mode estate of
        E.Edit _  -> editView
        E.Control -> controlView

testStart :: IO ()
testStart = do
    vty <- mkVty
    let cs = C.fromList [ 100 ~> E.quit ]
    eventloop vty E.defaultEditState cs
    reserve_display $ terminal vty
    shutdown vty
  where
    (~>) = (,)
