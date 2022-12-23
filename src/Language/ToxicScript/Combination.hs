module Language.ToxicScript.Combination where

import Language.ToxicScript.Expr

getCombiner :: Expr -> Either String Expr
getCombiner (List xs) | not (null xs) = Right . List $ init xs
getCombiner expr = Left $  "`" ++ show expr ++ "` is not a valid combination."

getParam :: Expr -> Either String Expr
getParam (List xs) | not (null xs) = Right $ last xs
getParam expr = Left $  "`" ++ show expr ++ "` is not a valid combination."

getCombination :: Expr -> Either String (Expr, Expr)
getCombination expr = (,) <$> getCombiner expr <*> getParam expr

getSymbol :: Expr -> Either String Symbol
getSymbol (Atom a) = Right a
getSymbol e = Left $ "Not a symbol: " ++ show e
