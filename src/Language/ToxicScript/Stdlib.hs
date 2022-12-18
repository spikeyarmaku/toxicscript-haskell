{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

module Language.ToxicScript.Stdlib where

import Control.Monad.Except ( throwError )
import Control.Monad.State  ( get )

import qualified Data.Text  as T

import Language.ToxicScript.Eval
import Language.ToxicScript.Env
import Language.ToxicScript.Expr

-- TODO consider using functions instead of bools
-- (true -> `first`, false -> `second`)
mkGlobalEnv
    :: (Num n, RealFrac n, Eq a)
    => (Rational -> a) -> (T.Text -> a) -- For parsing
    -> (a -> n) -> (n -> a)         -- to/from numerical types
    -> (a -> Bool) -> (Bool -> a)   -- to/from bools
    -> Env (Value a)
mkGlobalEnv fromRat fromText toNum fromNum toBool fromBool =
    extendEnvMany (stringsAndNumbers (Opaque . fromRat) (Opaque . fromText))
        [ addValue "lambda"     lambdaTr
        -- , addValue "let1"       let1Tr
        -- , addValue "letrec1"    letrec1Tr
        , addValue "let"        letTr
        , addValue "letrec"     letrecTr
        , addValue "list"       listTr
        , addValue "cons"       consTr
        
        , addValue "map"        mapTr
        , addValue "nth"        $ nthTr (round . toNum)
        , addValue "if"         $ ifTr toBool
        , addValue "+"          $ mathTr toNum fromNum (+)
        , addValue "*"          $ mathTr toNum fromNum (*)
        , addValue "-"          $ mathTr toNum fromNum (-)
        , addValue "/"          $ mathTr toNum fromNum (/)
        , addValue "="          $ eqTr fromBool

        , addValue "pi"         $ Opaque $ fromNum 3.141592653589
        , addValue "e"          $ Opaque $ fromNum 2.718281828459
        
        , (List [], emptyTr)
        ]

mapTr :: Value a
mapTr = curryTr 2 $ Transform $ \[f, lstExpr] -> do
    lst <- eval lstExpr
    case lst of
        Promise env' (List xs) ->
            let toMappedElem expr = List [f, expr]
            in  pure $ Promise env' (List (map toMappedElem xs))
        _ -> throwError "map: Not a list"

emptyTr :: Value a
emptyTr = Promise emptyEnv (List [])

lambdaTr :: Value a
lambdaTr = Transform $ \[name, body] -> do
    staticEnv <- get
    pure $ Transform $ \[value] -> do
        val <- eval value
        withEnv (extendEnv name val staticEnv) $ eval body

consTr :: Value a
consTr = Transform $ \[x, xs] -> do
    rest <- eval xs
    case rest of
        Promise _ (List listTail) -> callTransform listTr [List (x:listTail)]
        _ -> throwError $ "Not a valid const: " ++ show rest

listTr :: Value a
listTr = Transform $ \elems -> do
    env <- get
    case elems of
        [List xs] -> pure $ Promise env (List xs)
        _ -> throwError $ "Invalid list: " ++ show elems

nthTr :: (a -> Int) -> Value a
nthTr toNum = Transform $ \[n, lstExpr] -> do
    lst <- eval lstExpr
    ni <- eval n >>=
        \case
            Opaque v -> pure $ toNum v
            _ -> throwError "First argument does not evaluate to a number"
    getNth ni lst

getNth :: Int -> Value a -> Toxic a
getNth n (Promise env (List xs)) = do
    withEnv env $ eval (xs !! n)
getNth _ v = throwError $ "Not a list: " ++ show v

-- -- Read all the parameters.
-- -- If the number of parameters is less then it needs, return a new transform.
-- -- If the number of parameters is equal to how many it needs, evaluate the
-- -- underlying function.
-- -- Otherwise, throw an error.
curryTr :: Int -> Value a -> Value a
curryTr = curryTr' []
    where
    curryTr' args argCount tr = Transform $ \params -> do
        let newArgs = args ++ params
        -- logStr $ "newArgs: " ++ show newArgs ++ " argCount: " ++ show argCount
        if length newArgs < argCount
            then pure $ curryTr' (args ++ params) argCount tr
            else if length newArgs == argCount
                then callTransform tr newArgs
                else throwError "Too many arguments"

toCurriedFunc :: Int -> ([a] -> a) -> Value a
toCurriedFunc n f = curryTr n $ Transform $ \params -> do
    args <- mapM eval params
    let isOpaque (Opaque _) = True
        isOpaque _ = False
        fromOpaque (Opaque o) = o
        fromOpaque _ = error "Not an opaque value"
    if all isOpaque args
        then pure . Opaque $ f (map fromOpaque args)
        else throwError "Error"

mathTr :: Num n => (a -> n) -> (n -> a) -> (n -> n -> n) -> Value a
mathTr toNum fromNum f =
    toCurriedFunc 2 $ \[x, y] -> fromNum $ f (toNum x) (toNum y)

eqTr :: Eq a => (Bool -> a) -> Value a
eqTr fromBool = toCurriedFunc 2 $ \[a, b] -> fromBool (a == b)

ifTr :: (a -> Bool) -> Value a
ifTr toBool = Transform $ \[cond, ifTrue, ifFalse] -> do
    cond' <-
        eval cond >>= 
            \case
                Opaque x -> pure $ toBool x
                _ -> throwError $ "if: Cannot evaluate: " ++ show cond
    eval (if cond' then ifTrue else ifFalse)

letTr :: Value a
letTr = Transform $ \[bindingList, body] -> do
    env <- get
    case bindingList of
        List bindings ->
            case bindings of
                [] -> eval body
                (List [name, value]:rest) -> do
                    v <- eval value
                    withEnv (extendEnv name v env) $
                        callTransform letTr [List rest, body]
                _ -> throwError "let: Invalid binding in binding list"
        _ -> throwError "let: Invalid binding list"

letrecTr :: Value a
letrecTr = Transform $ \[bindingList, body] -> do
    case bindingList of
        List bindings ->
            case bindings of
                [] -> eval body
                (List [name, value]:rest) -> mdo
                    env <- get
                    -- let newEnv = extendEnv name (Promise newEnv value) env
                    let newEnv = extendEnv name v env
                    v <- withEnv newEnv $ eval value
                    withEnv newEnv $ callTransform letrecTr [List rest, body]
                _ -> throwError "let: Invalid binding in binding list"
        _ -> throwError "let: Invalid binding list"

-- let1Tr :: Value a
-- let1Tr = Transform $ \[name, value, body] -> do
--     tr <- callTransform lambdaTr [name, body]
--     callTransform tr [value]

-- letrec1Tr :: Value a
-- letrec1Tr = Transform $ \[name, value, body] -> mdo
--     modify (extendEnv name v)
--     v <- eval value
--     eval body

