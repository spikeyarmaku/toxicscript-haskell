module Language.ToxicScript.Combination where

import Language.ToxicScript.Expr

getCombiner :: Expr -> Either String Expr
getCombiner (List (x:_)) = Right x
getCombiner expr = Left $  "`" ++ show expr ++ "` is not a valid combination."

getParams :: Expr -> Either String [Expr]
getParams (List (_:xs)) = Right xs
getParams expr = Left $  "`" ++ show expr ++ "` is not a valid combination."

getCombination :: Expr -> Either String (Expr, [Expr])
getCombination expr = (,) <$> getCombiner expr <*> getParams expr

isValidCombination :: Expr -> Bool
isValidCombination (List (_:_)) = True
isValidCombination _ = False
