{-# LANGUAGE LambdaCase #-}

module Language.ToxicScript.Env where

import Control.Applicative  ((<|>))

import qualified Data.Text  as T
import qualified Data.Map   as M

import Language.ToxicScript.Expr
import Language.ToxicScript.Parse

data Env a
    = Env
        { convertExpr   :: Expr -> Maybe a
        , lookupExpr    :: M.Map Expr a }

instance Show (Env a) where
    show (Env _ es) = show $ M.keys es

emptyEnv :: Env a
emptyEnv = Env (const Nothing) M.empty

-- Given two functions to convert `Rational` and `Text` to `a`, provide a
-- default environment to parse numbers and strings
stringsAndNumbers :: (Rational -> a) -> (T.Text -> a) -> Env a
stringsAndNumbers convertRat convertStr =
    Env (\case
            (Atom (Symbol s)) ->
                case getRational s of
                    Nothing -> convertStr <$> getString s
                    Just r -> Just $ convertRat r
            _ -> Nothing) M.empty

lookupEnv :: Env a -> Expr -> Maybe a
lookupEnv env expr = (lookupExpr env M.!? expr) <|> convertExpr env expr

extendEnv :: Expr -> a -> Env a -> Env a
extendEnv expr val (Env cv lu) = Env cv $ M.insert expr val lu

extendEnvMany :: Env a -> [(Expr, a)] -> Env a
extendEnvMany = foldr (\(name, val) env -> extendEnv name val env)
