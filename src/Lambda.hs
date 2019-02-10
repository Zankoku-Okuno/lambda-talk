module Lambda where

import Data.String
import Data.List


newtype Var = V String
    deriving(Eq, Read)

data Expr
    = Var Var
    | Lam Var Expr
    | App Expr Expr
    deriving(Read)

data ExprCtx
    = Hole
    | LamCtx Var ExprCtx
    | AppCtxL ExprCtx Expr
    | AppCtxR Expr ExprCtx

data Redex
    = Alpha Expr
    | Beta Expr
    | Eta Expr
    deriving(Show)


fvs :: Expr -> [Var]
fvs (Var x) = [x]
fvs (Lam x e) = fvs e \\ [x]
fvs (App f e) = nub $ fvs f ++ fvs e

redexes :: Expr -> [(Redex, ExprCtx)]
redexes (Var _) = []
redexes (Lam x e) = alpha ++ beta ++ eta
    where
    alpha = [(Alpha $ Lam x e, Hole)]
    beta = [(e', LamCtx x r) | (e', r) <- redexes e]
    eta = case e of
        App e1 (Var y) | x == y && x `notElem` fvs e1 -> [(Eta $ Lam x e, Hole)]
        _ -> []
redexes (App f e) = redexHere f ++ redexL ++ redexR
    where
    redexHere (Lam _ _) = [(Beta $ App f e, Hole)]
    redexHere _ = []
    redexL = [(f', AppCtxL r e) | (f', r) <- redexes f]
    redexR = [(e', AppCtxR f r) | (e', r) <- redexes e]
    -- interleave (x:xs) (y:ys) = x : y : interleave xs ys
    -- interleave xs [] = xs
    -- interleave [] ys = ys

alpha :: Expr -> Var -> Expr
alpha (Lam x e) x' = Lam x' $ hygenicSubst (x, Var x') e
beta :: Expr -> Expr
beta (App (Lam x e) e') = hygenicSubst (x, e') e
eta :: Expr -> Expr
eta (Lam _ (App e _)) = e

-- FIXME not hygenic
hygenicSubst :: (Var, Expr) -> Expr -> Expr
hygenicSubst (x, e') (Var y)
    | x == y = e' 
    | otherwise = Var y
hygenicSubst (x, e') (Lam y e)
    -- FIXME x /= y and y not in fv(e')
    = Lam y (hygenicSubst (x, e') e)
hygenicSubst (x, e') (App f e)
    = App (hygenicSubst (x, e') f) (hygenicSubst (x, e') e)

replace :: ExprCtx -> Expr -> Expr
replace Hole e' = e'
replace (LamCtx x h) e' = Lam x (replace h e')
replace (AppCtxL h e) e' = App (replace h e') e
replace (AppCtxR f h) e' = App f (replace h e')




instance Show Var where
    show (V x) = x
instance Show Expr where
    show (Var x) = show x
    show (Lam x e) = concat ["λ", show x, ". ", show e]
    show (App f e) = concat [parenL f, " ", parenR e]
instance Show ExprCtx where
    show Hole = "□"
    show (LamCtx x h) = concat ["λ", show x, ". ", show h]
    show (AppCtxL h e) = concat [parenL h, " ", parenR e]
        where
        parenL h@(LamCtx _ _) = addParens (show h)
        parenL h = show h
    show (AppCtxR f h) = concat [parenL f, " ", parenR h]
        where
        parenR h@Hole = show h
        parenR h = addParens (show h)
parenL f@(Lam _ _) = addParens (show f)
parenL f = show f
parenR e@(Var _) = show e
parenR e = addParens (show e)

instance IsString Var where fromString = V
instance IsString Expr where fromString = Var . V

(<:) :: [a] -> a -> [a]
xs <: x = xs ++ [x]

addParens str = concat ["(", str, ")"]