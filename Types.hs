module Types where

import Control.Monad

import Text.PrettyPrint.ANSI.Leijen

data Tree p
    = Hole
    | Focus (Tree p) -- To be able to prettyprint, will hopefully go away later
    | Node p [Tree p]
    deriving (Eq, Show)

data InvTree p
    = InvNode p [Tree p] [Tree p]
    deriving (Eq, Show)

type Trace p = [InvTree p]
type Context p = (Trace p, Tree p)

type M = Maybe

bet :: [a] -> [a] -> a -> [a]
bet ls rs x = reverse ls ++ x : rs

moveUp :: Context p -> M (Context p)
moveUp ([], _) = mzero
moveUp (c:cs, tree) = case c of
    InvNode p pr ne -> return (cs, Node p $ bet pr ne tree)

moveDown :: Context p -> M (Context p)
moveDown (cs, tree) = case tree of
    Hole -> mzero
    Focus t -> mzero -- Should not occur
    Node p [] -> mzero
    Node p (x : xs) -> return (InvNode p [] xs : cs, x)

moveLeft :: Context p -> M (Context p)
moveLeft ([], _) = mzero
moveLeft (c : cs, tree) = case c of
    InvNode p [] ne -> mzero
    InvNode p (x:pr) ne -> return (InvNode p pr (tree:ne) : cs, x)

moveRight :: Context p -> M (Context p)
moveRight ([], _) = mzero
moveRight (c : cs, tree) = case c of
    InvNode p pr [] -> mzero
    InvNode p pr (x:ne) -> return (InvNode p (tree:pr) ne : cs, x)

appendLeft :: Context p -> M (Context p)
appendLeft ([], _) = mzero
appendLeft (c : cs, t) = case c of
    InvNode p pr ne -> return (InvNode p pr (t:ne) : cs, Hole)

appendRight :: Context p -> M (Context p)
appendRight ([], _) = mzero
appendRight (c : cs, t) = case c of
    InvNode p pr ne -> return (InvNode p (t:pr) ne : cs, Hole)

transform :: Trace p -> Tree p -> M (Tree p)
transform [] f = return f
transform (c : cs) f = transform cs =<< case c of
    InvNode p ls rs -> return $ Node p $ bet ls rs f

delete :: Context p -> M (Context p)
delete (c : cs, Hole) = case c of
    InvNode p ls rs -> return (cs, Node p $ reverse ls ++ rs)
delete (c : cs, _) = return (c : cs, Hole)
delete ([], _) = return ([], Hole)

-- --
-- PrettyPrinting
-- --


focus :: Doc -> Doc
focus x = f lbracket <> x <> f rbracket
    where f = bold . red

prTree :: (p -> [Doc] -> Doc) -> Tree p -> Doc
prTree pr tree = case tree of
    Hole -> prHole
    Focus tree' -> focus $ prTree pr tree'
    Node p ps -> pr p (map (prTree pr) ps)

prHole :: Doc
prHole = cyan $ text "<?>"
