{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

module Language.ToxicScript.Stdlib where

import Control.Monad.Except ( throwError )

import Language.ToxicScript.Eval
import Language.ToxicScript.Env
import Language.ToxicScript.Expr

--  ($define! list-tail
--      ($lambda (ls k)
--          ($if (>? k 0)
--              (list-tail (cdr ls) (- k 1))
--              ls)))

emptyTr :: Value a
emptyTr = Promise emptyEnv (List [])

lambdaTr :: Value a
lambdaTr = Transform $ \staticEnv [name, body] -> do
    pure $ Transform $ \dynEnv [value] -> do
        val <- eval dynEnv value
        eval (extendEnv name val staticEnv) body

consTr :: Value a
consTr = Transform $ \env [x, xs] -> do
    rest <- eval env xs
    case rest of
        Promise _ (List listTail) -> callTransform listTr env [List (x:listTail)]
        _ -> throwError $ "Not a valid const: " ++ show rest
    -- rest <- eval env xs
    -- logStr $ "Evaluating " ++ show xs ++ " to " ++ show rest
    -- case rest of
    --     Promise _ lstTail -> pure $ Promise env (List [x, lstTail])
    --     v -> throwError $ "Not a valid const: " ++ show v

listTr :: Value a
listTr = Transform $ \env elems ->
    case elems of
        [List xs] -> pure $ Promise env (List xs)
        _ -> throwError $ "Invalid list: " ++ show elems

nthTr :: (a -> Int) -> Value a
nthTr toNum = Transform $ \env [n, lstExpr] -> do
    lst <- eval env lstExpr
    ni <- eval env n >>=
        \case
            Opaque v -> pure $ toNum v
            _ -> throwError "First argument does not evaluate to a number"
    getNth ni lst

getNth :: Int -> Value a -> Eval (Value a)
getNth n (Promise env (List xs)) = eval env (xs !! n)
getNth _ v = throwError $ "Not a list: " ++ show v

-- -- Read all the parameters.
-- -- If the number of parameters is less then it needs, return a new transform.
-- -- If the number of parameters is equal to how many it needs, evaluate the
-- -- underlying function.
-- -- Otherwise, throw an error.
curryTr :: Int -> Value a -> Value a
curryTr = curryTr' []
    where
    -- curryTr' env args 0 tr = tr env args
    curryTr' args argCount tr = Transform $ \env params -> do
        let newArgs = args ++ params
        logStr $ "newArgs: " ++ show newArgs ++ " argCount: " ++ show argCount
        if length newArgs < argCount
            then pure $ curryTr' (args ++ params) argCount tr
            else if length newArgs == argCount
                then callTransform tr env newArgs
                else throwError "Too many arguments"

mathTr :: Num n => (a -> n) -> (n -> a) -> (n -> n -> n) -> Value a
mathTr toNum fromNum f = curryTr 2 $ Transform $ \env [x, y] -> do
    v1 <- eval env x
    v2 <- eval env y
    pure $ mathOp f v1 v2
    where
    -- mathOp :: (n -> n -> n) -> Value a -> Value a -> Value a
    mathOp op (Opaque xv) (Opaque yv) =
        Opaque $ fromNum $ op (toNum xv) (toNum yv)
    mathOp _ _ _ = error "mathOp: invalid argument"

-- mathTr :: Num n => (a -> n) -> (n -> a) -> (n -> n -> n) -> Value a
-- mathTr toNum fromNum f = Transform $ \env [x, y] -> do
--     v1 <- eval env x
--     v2 <- eval env y
--     pure $ mathOp f v1 v2
--     where
--     -- mathOp :: (n -> n -> n) -> Value a -> Value a -> Value a
--     mathOp op (Opaque xv) (Opaque yv) =
--         Opaque $ fromNum $ op (toNum xv) (toNum yv)
--     mathOp _ _ _ = error "mathOp: invalid argument"

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

letTr :: Value a
letTr = Transform $ \env [name, value, body] -> do
    tr <- callTransform lambdaTr env [name, body]
    callTransform tr env [value]

-- letrecTr :: Value a
-- letrecTr = Transform $ \env [name, value, body] -> do
--     let newEnv = extendEnv name (Promise newEnv value) env
--     eval newEnv body

letrecTr :: Value a
letrecTr = Transform $ \env [name, value, body] -> mdo
    let newEnv = extendEnv name v env
    v <- eval newEnv value
    eval newEnv body

