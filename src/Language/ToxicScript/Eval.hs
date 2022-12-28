module Language.ToxicScript.Eval where

import Language.ToxicScript.Expr
import Language.ToxicScript.Combination
import Language.ToxicScript.Env

data Term a
    = Val a                                 -- User-defined value
    | Var Expr                              -- Variable
    | Abs (Env (Term a) -> Expr -> Term a)  -- Lambda abstraction
    -- Application is handled explicitly by the syntax (see `evalExpr` and
    -- `apply`)

showTerm :: Show a => Term a -> String
showTerm (Val a)        = "<<Value: " ++ show a ++ ">>"
showTerm (Var v)        = "<<Variable: " ++ show v ++ ">>"
showTerm (Abs _)      = "<<Abstraction>>"

instance Show (Term a) where
    show = defaultPrint

defaultPrint :: Term a -> [Char]
defaultPrint (Val _) = "Val _"
defaultPrint (Var v) = "Var " ++ show v
defaultPrint (Abs _) = "Abs"

data Result a
    = Result
        { getResult :: Either String a
        , getLog    :: String }
    deriving (Show)

eval :: Env (Term a) -> Term a -> Term a
eval _ (Val a) = Val a
eval env (Var v) =
    case lookupEnv env v of
        Nothing -> Var v
        Just x -> x
eval _ (Abs f) = Abs f

apply :: Env (Term a) -> Term a -> Expr -> Term a
apply env (Abs f) v = f env v
apply env t1 t2 = apply env (eval env t1) t2

evalExpr :: Env (Term a) -> Expr -> Term a
evalExpr env x =
    case lookupEnv env x of
        Nothing ->
            case getCombination x of
                Atomic sym -> error $ "Unassigned variable: " ++ show sym
                Combination c p ->
                    case c of
                        List [] -> evalExpr env p
                        _       -> apply env (evalExpr env c) p
        Just v -> v
