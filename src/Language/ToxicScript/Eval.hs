module Language.ToxicScript.Eval where

import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.State

import Language.ToxicScript.Expr
import Language.ToxicScript.Combination
import Language.ToxicScript.Env

--              log             env             error
type Eval a b = ExceptT String (StateT (Env a) (Writer String)) b

type Toxic a = Eval (Value a) (Value a)

logStr :: String -> Eval a ()
logStr str = tell $ "[INFO] " ++ str ++ "\n"

data Value a
    = Transform ([Expr] -> Toxic a)
    | Promise (Env (Value a)) Expr
    | Opaque a

callTransform :: Value a -> [Expr] -> Toxic a
callTransform (Transform tr) args = tr args
callTransform _ _ = throwError "Not a transform"

addValue :: String -> Value a -> (Expr, Value a)
addValue name tr = (mkSymbol name, tr)

instance Show (Value a) where
    show (Transform _) = "<<Transform>>"
    show (Promise _ e) = "<<Promise: " ++ show e ++ ">>"
    show (Opaque _) = "<<Opaque value>>"

data Result a
    = Result
        { getResult :: Either String a
        , getLog    :: String }

runEval :: Env (Value a) -> Expr -> Result (Value a)
runEval env expr =
    let ((result, _), logMsg) =
            runWriter (runStateT (runExceptT (eval expr)) env)
    in  Result result logMsg

eval :: Expr -> Toxic a
eval expr = do
    -- Check if the expression has a value assigned to it
    logStr $ "Evaluating: `" ++ show expr ++ "`"
    env <- get
    case lookupEnv env expr of
        Nothing -> do
            -- If not, check if it is a valid combination
            if isValidCombination expr
                -- If it is, evaluate the combiner, and apply the parameters
                then do
                    case getCombination expr of
                        Left e -> throwError e
                        Right (combiner, params) -> do
                            val <- eval combiner
                            evalCombination val params
                else throwError $ "No value assigned to " ++ show expr
        Just v -> pure v
            -- case v of
            --     Promise env' expr' -> withEnv env' $ eval expr'
            --     _ -> pure v

withEnv :: Env a -> Eval a b -> Eval a b
withEnv env comp = do
    s <- get
    put env
    v <- comp
    put s
    pure v

evalCombination :: Value a -> [Expr] -> Toxic a
evalCombination val params = do
    case val of
        Transform t -> t params
        Promise env' e -> do
            p <- withEnv env' $ eval e
            evalCombination p params
        Opaque o ->
            if null params
                then pure $ Opaque o
                else throwError $ "Too many parameters: " ++ show params
