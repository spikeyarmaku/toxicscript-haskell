module Language.ToxicScript.Combination where

import Language.ToxicScript.Expr

data CompoundExpr = Atomic Symbol | Combination Expr Expr

getCombiner :: Expr -> Either String Expr
getCombiner (List xs) | not (null xs) = Right . List $ init xs
getCombiner expr = Left $  "`" ++ show expr ++ "` is not a valid combination."

getParam :: Expr -> Either String Expr
getParam (List xs) | not (null xs) = Right $ last xs
getParam expr = Left $  "`" ++ show expr ++ "` is not a valid combination."

getCombination :: Expr -> CompoundExpr
getCombination expr =
    let sym = getSymbol expr
        com = (,) <$> getCombiner expr <*> getParam expr
    in  case com of
            Left _ ->
                case sym of
                    Left _ -> error "Impossible"
                    Right s -> Atomic s
            Right (c, p) -> Combination c p

getSymbol :: Expr -> Either String Symbol
getSymbol (Atom a) = Right a
getSymbol e = Left $ "Not a symbol: " ++ show e
