module Language.ToxicScript.Eval where

import Language.ToxicScript.Expr
import Language.ToxicScript.Combination
import Language.ToxicScript.Env

data Term a
    = Val a                                 -- User-defined value
    | Exp Expr                              -- Expression (variable or code)
    | Abs (Env (Term a) -> Expr -> Term a)  -- Lambda abstraction
    -- Application is handled explicitly by the syntax (see `evalExpr` and
    -- `apply`)
    -- Furthermore, `App t1 t2` cannot be written, as `Abs` handles syntax as
    -- well as semantics, therefore an expression cannot readily be compiled
    -- into a `Term`. Consider e.g. a `let`-binding, where the first argument
    -- must not be evaluated, but needs to be treated as a name.
    -- Important note: the environment cannot be a first-class object, since
    -- it doesn't even exist from the viewpoint of semantics - it is just an
    -- optimization technique.

-- emptyTerm = ()
emptyTerm :: Term a
emptyTerm = Exp (List [])

showTerm :: Show a => Term a -> String
showTerm (Val a)    = "<<Value: " ++ show a ++ ">>"
showTerm (Exp v)    = "<<Expression: " ++ show v ++ ">>"
showTerm (Abs _)    = "<<Abstraction>>"

instance Show (Term a) where
    show = defaultPrint

defaultPrint :: Term a -> [Char]
defaultPrint (Val _) = "Val _"
defaultPrint (Exp v) = "Exp " ++ show v
defaultPrint (Abs _) = "Abs"

data Result a
    = Result
        { getResult :: Either String a
        , getLog    :: String }
    deriving (Show)

eval :: Env (Term a) -> Term a -> Term a
eval _ (Val a) = Val a
eval env (Exp v) =
    case lookupEnv env v of
        Nothing -> Exp v
        Just x -> x
eval _ (Abs f) = Abs f

apply :: Env (Term a) -> Term a -> Expr -> Term a
apply _   (Val _) _ = error "Cannot use value as a function"
apply env (Exp v) e =
    case eval env (Exp v) of
        Exp v' ->
            if v == v'
                then error $ "Cannot evaluate expression " ++ show v
                else apply env (Exp v') e
        x -> apply env x e
apply env (Abs f) v = f env v

evalExpr :: Env (Term a) -> Expr -> Term a
evalExpr env x =
    case lookupEnv env x of
        Nothing ->
            case getCombination x of
                Empty -> emptyTerm
                Atomic sym -> error $ "Unassigned variable: " ++ show sym
                Combination c p ->
                    case c of
                        List [] -> evalExpr env p
                        _       -> apply env (evalExpr env c) p
        Just v -> v
