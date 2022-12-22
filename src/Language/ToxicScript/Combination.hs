module Language.ToxicScript.Combination where

import Language.ToxicScript.Expr

getCombiner :: Expr -> Either String Expr
getCombiner (List xs) | not (null xs) = Right . List $ init xs
getCombiner expr = Left $  "`" ++ show expr ++ "` is not a valid combination."

getParams :: Expr -> Either String Expr
getParams (List xs) | not (null xs) = Right $ last xs
getParams expr = Left $  "`" ++ show expr ++ "` is not a valid combination."

getCombination :: Expr -> Either String (Expr, Expr)
getCombination expr = (,) <$> getCombiner expr <*> getParams expr

getSymbol :: Expr -> Either String Symbol
getSymbol (Atom a) = Right a
getSymbol e = Left $ "Not a symbol: " ++ show e
