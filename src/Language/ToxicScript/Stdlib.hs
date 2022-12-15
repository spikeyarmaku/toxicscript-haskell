{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

module Language.ToxicScript.Stdlib where

import Control.Monad.Except ( throwError )

import Language.ToxicScript.Eval
import Language.ToxicScript.Env
import Language.ToxicScript.Expr

consTr :: Expr -> Value a
consTr cr = Transform $ \env params -> pure $ Promise env (List (cr:params))

listTr :: Value a
listTr = Transform $ \env [lst] -> pure $ Promise env lst

nthTr :: (a -> Int) -> Expr -> Value a
nthTr toNum cr = Transform $ \env [n, lstExpr] -> do
    lst <- eval env lstExpr
    ni <- eval env n >>=
            \case
                Opaque v -> pure $ toNum v
                _ -> throwError $ show cr ++ ": First argument does not evaluate " ++
                        "to a number"
    case lst of
        Promise env' (List (_:listTail)) ->
            case listTail of
                [] -> throwError "Not enough elements in list"
                (val:rest) ->
                    if ni <= 0
                        then eval env' val
                        else
                            let newExpr =
                                    List (cr:mkSymbol (show (ni - 1)):rest)
                            in  eval env' newExpr
        _ -> throwError "Not a list"

mathTr :: Num n => (a -> n) -> (n -> a) -> (n -> n -> n) -> Value a
mathTr toNum fromNum f = Transform $ \env [x, y] -> do
    v1 <- eval env x
    v2 <- eval env y
    pure $ mathOp f v1 v2
    where
    -- mathOp :: (n -> n -> n) -> Value a -> Value a -> Value a
    mathOp op (Opaque x) (Opaque y) = Opaque $ fromNum $ op (toNum x) (toNum y)
    mathOp _ _ _ = error "mathOp: invalid argument"

eqTr :: Eq a => (Bool -> a) -> Value a
eqTr fromBool = Transform $ \env [a, b] -> do
    a' <-
        eval env a >>=
            \case
                Opaque e -> pure e
                x -> throwError $ "eq: Cannot evaluate first operand: " ++ show x
    b' <-
        eval env b >>=
            \case
                Opaque e -> pure e
                x -> throwError $ "eq: Cannot evaluate second operand: " ++   
                        show x
    pure $ Opaque (fromBool (a' == b'))

ifTr :: (a -> Bool) -> Value a
ifTr toBool = Transform $ \env [cond, ifTrue, ifFalse] -> do
    cond' <-
        eval env cond >>= 
            \case
                Opaque x -> pure $ toBool x
                _ -> throwError $ "if: Cannot evaluate: " ++ show cond
    eval env (if cond' then ifTrue else ifFalse)

lambdaTr :: Value a
lambdaTr = Transform $ \env [name, body] -> do
    pure $ Transform $ \dynEnv [value] -> do
        val <- eval dynEnv value
        let newEnv = extendEnv name val env
        eval newEnv body

letTr :: Value a
letTr = Transform $ \env [name, value, body] ->
    eval env (List [List [mkSymbol "lambda", name, body], value])

letrecTr :: Value a
letrecTr = Transform $ \env [name, value, body] -> mdo
    let newEnv = extendEnv name v env
    v <- eval newEnv value
    eval newEnv body
