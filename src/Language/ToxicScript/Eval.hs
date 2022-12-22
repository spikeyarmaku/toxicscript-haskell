module Language.ToxicScript.Eval where

import Language.ToxicScript.Expr
import Language.ToxicScript.Combination
import Language.ToxicScript.Env

import Debug.Trace

data Term a
    = Val a                                 -- User-defined value
    | Var Symbol                            -- Variable
    | Abs (Env (Term a) -> Expr -> Term a)
    -- | Cls (Env (Term a)) Expr (Term a)      -- Closure
    -- | Syn (Env (Term a) -> Expr -> Term a)  -- Syntax definition
    -- | App (Term a) (Term a)                 -- Application
    -- | Exp Expr                              -- Unevaluated expression

showTerm :: Show a => Term a -> String
showTerm (Val a)        = "<<Value: " ++ show a ++ ">>"
showTerm (Var v)        = "<<Variable: " ++ show v ++ ">>"
-- showTerm (Exp e)        = "<<Expression: " ++ show e ++ ">>"
showTerm (Abs _)      = "<<Abstraction>>"
-- showTerm (Cls _ e t)    = "<<Closure: (" ++ show e ++ ") " ++ show t ++ ">>"
-- showTerm (Cls _)    = "<<Closure>>"
-- showTerm (App e1 e2)    = "<<Application: (" ++ show e1 ++ ") (" ++ show e2 ++ ")>>"
-- showTerm (Syn _)        = "<<Syntax definition>>"

instance Show (Term a) where
    show = defaultPrint

defaultPrint :: Term a -> [Char]
defaultPrint (Val _) = "Val _"
defaultPrint (Var v) = "Var " ++ show v
-- defaultPrint (Exp e) = "Exp " ++ show e
defaultPrint (Abs _) = "Abs"
-- defaultPrint (App e1 e2) =
    -- "App (" ++ defaultPrint e1 ++ ") (" ++ defaultPrint e2 ++ ")"
-- defaultPrint (Abs _) = "Abs <<Term a -> Term a>>"
-- defaultPrint (Cls _ n b) = "Cls " ++ show n ++ " (" ++ defaultPrint b ++ ")"
-- defaultPrint (Cls _) = "Cls <<Env (Term a) -> Expr -> Term a>>"
-- defaultPrint (Syn _) = "Syn <<Env (Term a) -> Expr -> Term a>>"

data Result a
    = Result
        { getResult :: Either String a
        , getLog    :: String }
    deriving (Show)

eval :: Env (Term a) -> Term a -> Term a
eval _      (Val a)         = Val a
eval env    (Var v)         =
    case lookupEnv env (Atom v) of
        Nothing -> Var v
        Just x -> x
eval _      (Abs f)       = Abs f
-- eval _      (Syn f)         = Syn f
-- eval env    (Exp x)         = evalExp env x
-- eval env    (App t1 t2)     = apply env t1 t2

apply :: Env (Term a) -> Term a -> Expr -> Term a
-- apply env (Syn f) (Exp e) = f env e
apply env (Abs f) v = f env v
apply env t1 t2 = apply env (eval env t1) t2

evalExpr :: Env (Term a) -> Expr -> Term a
evalExpr env x = -- trace ("evalExp " ++ show x) $
    case lookupEnv env x of
        Nothing -> -- trace "  No value assigned" $
            case getCombination x of
                Left _ -> -- trace "  Not a combination" $
                    case getSymbol x of
                        Left e -> error $ "PANIC! " ++ e
                        Right s -> error $ "Unassigned variable: " ++ show s
                Right (List [], p) -> -- trace "  Singular combination" $
                    evalExpr env p
                Right (c, p) -> -- trace "  Combination" $
                    apply env (evalExpr env c) p
        Just v -> -- trace "  Value assigned"
            v
