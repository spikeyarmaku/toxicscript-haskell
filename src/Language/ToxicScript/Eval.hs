module Language.ToxicScript.Eval where

import Control.Monad.Writer
import Control.Monad.Except

import Language.ToxicScript.Expr
import Language.ToxicScript.Combination
import Language.ToxicScript.Env

--

type Eval = ExceptT String (Writer String)

logStr :: String -> Eval ()
logStr str = tell ("[INFO] " ++ str ++ "\n")

data Value a
    = Transform (Env (Value a) -> [Expr] -> Eval (Value a))
    | Promise (Env (Value a)) Expr
    | Opaque a

callTransform :: Value a -> Env (Value a) -> [Expr] -> Eval (Value a)
callTransform (Transform tr) env args = tr env args
callTransform _ _ _ = throwError "Not a transform"

addValue :: String -> Value a -> (Expr, Value a)
addValue name tr = (mkSymbol name, tr)

instance Show (Value a) where
    show (Transform _) = "<<Transform>>"
    show (Promise _ e) = "<<Promise: " ++ show e ++ ">>"
    show (Opaque _) = "<<Opaque value>>"

eval :: Env (Value a) -> Expr -> Eval (Value a)
eval env expr = do
    -- Check if the expression has a value assigned to it
    logStr $ "Evaluating: `" ++ show expr ++ "`"
    case lookupEnv env expr of
        Nothing -> do
            -- If not, check if it is a valid combination
            if isValidCombination expr
                -- If it is, evaluate the combiner, and apply the parameters
                then do
                    case getCombination expr of
                        Left e -> throwError e
                        Right (combiner, params) ->
                            evalCombination env combiner params
                else throwError $ "No value assigned to " ++ show expr
        Just v -> pure v

evalCombination :: Env (Value a) -> Expr -> [Expr] -> Eval (Value a)
evalCombination env name params = do
    val <- eval env name
    case val of
        Transform t -> t env params
        Promise env' e -> evalCombination env' e params
        Opaque o ->
            if null params
                then pure $ Opaque o
                else throwError $ "Too many parameters: " ++ show params
