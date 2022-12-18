{-# LANGUAGE LambdaCase #-}

module Language.ToxicScript.Env where

import Language.ToxicScript.Expr
import Language.ToxicScript.Parse

import qualified Data.Text  as T

newtype Env a = Env (Expr -> Maybe a)

emptyEnv :: Env a
emptyEnv = Env $ const Nothing

-- Given two functions to convert `Rational` and `Text` to `a`, provide a
-- default environment to parse numbers and strings
stringsAndNumbers :: (Rational -> a) -> (T.Text -> a) -> Env a
stringsAndNumbers convertRat convertStr =
    Env $
        \case
            (Symbol s) ->
                case getRational s of
                    Nothing -> convertStr <$> getString s
                    Just r -> Just $ convertRat r
            _ -> Nothing

lookupEnv :: Env a -> Expr -> Maybe a
lookupEnv (Env rules) = rules

extendEnv :: Expr -> a -> Env a -> Env a
extendEnv expr val oldEnv =
    Env $ \e -> if e == expr then Just val else lookupEnv oldEnv e

extendEnvMany :: Env a -> [(Expr, a)] -> Env a
extendEnvMany = foldr (\(name, val) env -> extendEnv name val env)
