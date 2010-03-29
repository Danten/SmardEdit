module Moves where

import Control.Arrow
import Control.Applicative
import Simple

data ExprCTX
    = ECBin Ident Two Expr
    -- | ECIf Three (Expr t) (Expr t)
    | ECApp Two Expr
    | ECAbs Ident
    deriving (Eq, Show)

data Two = L | R deriving (Eq, Show)
data Three = T Two | C deriving (Eq, Show)

type ExprCtx = ([ExprCTX], Expr)

type Move = ExprCtx -> Maybe ExprCtx


moveUp :: Move
moveUp ([], _) = Nothing
moveUp (c:cs, e) = pure . (,) cs $ case c of
    ECBin t L e' -> EBin t e e'
    ECBin t R e' -> EBin t e' e
--    ECIf C e1 e2   -> EIf e1 e e2
--    ECIf (T L) e1 e2   -> EIf e e1 e2
--    ECIf (T R) e1 e2   -> EIf e1 e2 e
    ECApp L e' -> EApp e e'
    ECApp R e' -> EApp e' e
    ECAbs t -> EAbs t e

-- *** : (a -> b) -> (c -> d) -> (a, c) -> (b, d)

moveRight :: Move
moveRight ([], _) = Nothing
moveRight (c:cs, e) = ((:cs) *** id) <$> case c of
    ECBin t L e' -> Just (ECBin t R e, e')
    ECApp L e'   -> Just (ECApp R e, e')
    _ -> Nothing

moveLeft :: Move
moveLeft ([], _) = Nothing
moveLeft (c:cs, e) = ((:cs) *** id) <$> case c of
    ECBin t R e' -> pure (ECBin t L e, e')
    ECApp R e'   -> pure (ECApp L e, e')
    _ -> Nothing

moveDown :: Move
moveDown (cs, e) = ((:cs) *** id) <$> case e of
    EBin t e1 e2 -> pure (ECBin t L e2, e1)
    EApp e1 e2   -> pure (ECApp L e2, e1)
    EAbs t e     -> pure (ECAbs t, e)
    _ -> Nothing