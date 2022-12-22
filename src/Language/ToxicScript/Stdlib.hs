module Language.ToxicScript.Stdlib where

import qualified Data.Text  as T

import Language.ToxicScript.Eval
import Language.ToxicScript.Env
import Language.ToxicScript.Expr
import Language.ToxicScript.Parse

-- env = extendEnvMany (stringsAndNumbers (Val . VRat) (Val . VStr . T.unpack)) $
--     [ (mkSymbol "lambda", lambdaVal)
--     , fromText "true" "(lambda x (lambda y x))"
--     , fromText "false" "(lambda x (lambda y y))"
--     , fromText "pair" "(lambda x (lambda y (lambda f (f x y))))"
--     , fromText "fst" "(lambda p (p true))" 
--     , fromText "snd" "(lambda p (p false))"
--     , (mkSymbol "let", letVal) ]

fromCode :: Env (Term a) -> String -> String -> (Expr, Term a)
fromCode env name t =
    case parseExpr . T.pack $ t of
        Left e -> error e
        Right v -> (mkSymbol name, evalExpr env v)

-- TODO consider using functions instead of bools
-- (true -> `first`, false -> `second`)
mkGlobalEnv
    :: (Num n, RealFrac n, Eq a)
    => (Rational -> a) -> (T.Text -> a) -- For parsing
    -> (a -> n) -> (n -> a)         -- to/from numerical types
    -> Env (Term a)
mkGlobalEnv fromRat fromText toNum fromNum =
    extendEnvMany (stringsAndNumbers (Val . fromRat) (Val . fromText))
        [ (mkSymbol "lambda",   lambdaVal)
        , (mkSymbol "let",      letVal)
        , (mkSymbol "letrec",   letrecVal)
        , (mkSymbol "defs",     defsVal)

        -- , addValue "letrec1"    letrec1Tr
        -- , addValue "let"        letTr
        -- , addValue "letrec"     letrecTr
        -- , addValue "list"       listTr
        -- , addValue "cons"       consTr
        
        -- , addValue "map"        mapTr
        -- , addValue "nth"        $ nthTr (round . toNum)
        -- , addValue "if"         $ ifTr toBool
        , (mkSymbol "+",    mathVal toNum fromNum (+))
        , (mkSymbol "*",    mathVal toNum fromNum (*))
        , (mkSymbol "-",    mathVal toNum fromNum (-))
        , (mkSymbol "/",    mathVal toNum fromNum (/))
        , (mkSymbol "=",    eqVal)

        -- , addValue "pi"         $ Opaque $ fromNum 3.141592653589
        -- , addValue "e"          $ Opaque $ fromNum 2.718281828459
        
        -- , (List [], emptyTr)
        ]

-- \name body value -> App (Abs name (Var body)) value
lambdaVal :: Term a
lambdaVal = Abs $ \_ name -> Abs $ \staticEnv body -> Abs $ \dynEnv value ->
    lamVal staticEnv dynEnv name body value

lamVal :: Env (Term a) -> Env (Term a) -> Expr -> Expr -> Expr -> Term a
lamVal staticEnv dynEnv (Atom name) body value =
    evalExpr (extendEnv (Atom name) (evalExpr dynEnv value) staticEnv) body
lamVal env _ x _ _ = evalExpr env x

letVal :: Term a
letVal =
    Abs $ \_ name ->
        Abs $ \staticEnv value ->
            Abs $ \dynEnv body -> lamVal staticEnv dynEnv name body value

letrecVal :: Term a
letrecVal =
    Abs $ \_ name ->
        Abs $ \_ value ->
            Abs $ \dynEnv body ->
                let newEnv = extendEnv name (evalExpr newEnv value) dynEnv
                in  evalExpr newEnv body     

defsVal :: Term a
defsVal =
    Abs $ \env defs ->
        case defs of
            List pairs ->
                Abs $ \_ body ->
                    let newEnv =
                            foldr (\(List [name, value]) e ->
                                        extendEnv name (evalExpr env value) e)
                                env pairs
                    in  evalExpr newEnv body
            _ -> error "defs: incorrect binding list"

mathVal
    :: (Num n, RealFrac n) => (a -> n) -> (n -> a) -> (n -> n -> n) -> Term a
mathVal toNum fromNum op =
    Abs $ \_ n1 -> Abs $ \env n2 ->
        case evalExpr env n1 of
            Val x ->
                case evalExpr env n2 of
                    Val y -> Val . fromNum $ op (toNum x) (toNum y)
                    _ -> error "Not a number"
            _ -> error "Not a number"

trueVal :: Term a
trueVal = Abs $ \_ x -> Abs $ \env _ -> evalExpr env x

falseVal :: Term a
falseVal = Abs $ \_ _ -> Abs $ \env y -> evalExpr env y

eqVal :: Eq a => Term a
eqVal = Abs $ \_ e1 -> Abs $ \env e2 ->
    case evalExpr env e1 of
            Val x ->
                case evalExpr env e2 of
                    Val y -> if x == y then trueVal else falseVal
                    _ -> error "Not a value"
            _ -> error "Not a value"

-- letrecVal :: Term a
-- letrecVal = Syn $ \_ name -> Syn $ \_ value -> Syn $ \env body ->
--     App (Cls (extendEnv name (Exp value) env) name (Exp body)) (Exp value)

-- mathVal :: (Num n, RealFrac n) => (a -> n) -> (n -> a) -> (n -> n -> n) -> Term a
-- mathVal toNum fromNum op =
--     Syn $ \_ x -> Syn $ \env y -> Cls env 

-- addTr :: (Num n, RealFrac n) => (a -> n) -> (n -> a) -> Term a
-- addTr toNum fromNum =
--     Abs (mkSymbol "x") $
--         pure $ Abs (mkSymbol "y") $ do
--             x <- eval (Var (mkSymbol "x"))
--             y <- eval (Var (mkSymbol "y"))
--             case x of
--                 Val x' ->
--                     case y of
--                         Val y' -> pure . Val $ fromNum $ toNum x' + toNum y'
--                         _ -> error $ show y
--                 _ -> error $ show x

-- mapTr :: Value a
-- mapTr = curryTr 2 $ Transform $ \[f, lstExpr] -> do
--     lst <- eval lstExpr
--     case lst of
--         Promise env' (List xs) ->
--             let toMappedElem expr = List [f, expr]
--             in  pure $ Promise env' (List (map toMappedElem xs))
--         _ -> throwError "map: Not a list"

-- emptyTr :: Value a
-- emptyTr = Promise emptyEnv (List [])

-- lambdaTr :: Value a
-- lambdaTr = Transform $ \[name, body] -> do
--     staticEnv <- get
--     pure $ Transform $ \[value] -> do
--         val <- eval value
--         withEnv (extendEnv name val staticEnv) $ eval body

-- consTr :: Value a
-- consTr = Transform $ \[x, xs] -> do
--     rest <- eval xs
--     case rest of
--         Promise _ (List listTail) -> callTransform listTr [List (x:listTail)]
--         _ -> throwError $ "Not a valid const: " ++ show rest

-- listTr :: Value a
-- listTr = Transform $ \elems -> do
--     env <- get
--     case elems of
--         [List xs] -> pure $ Promise env (List xs)
--         _ -> throwError $ "Invalid list: " ++ show elems

-- nthTr :: (a -> Int) -> Value a
-- nthTr toNum = Transform $ \[n, lstExpr] -> do
--     lst <- eval lstExpr
--     ni <- eval n >>=
--         \case
--             Opaque v -> pure $ toNum v
--             _ -> throwError "First argument does not evaluate to a number"
--     getNth ni lst

-- getNth :: Int -> Value a -> Toxic a
-- getNth n (Promise env (List xs)) = do
--     withEnv env $ eval (xs !! n)
-- getNth _ v = throwError $ "Not a list: " ++ show v

-- -- -- Read all the parameters.
-- -- -- If the number of parameters is less then it needs, return a new transform.
-- -- -- If the number of parameters is equal to how many it needs, evaluate the
-- -- -- underlying function.
-- -- -- Otherwise, throw an error.
-- curryTr :: Int -> Value a -> Value a
-- curryTr = curryTr' []
--     where
--     curryTr' args argCount tr = Transform $ \params -> do
--         let newArgs = args ++ params
--         -- logStr $ "newArgs: " ++ show newArgs ++ " argCount: " ++ show argCount
--         if length newArgs < argCount
--             then pure $ curryTr' (args ++ params) argCount tr
--             else if length newArgs == argCount
--                 then callTransform tr newArgs
--                 else throwError "Too many arguments"

-- toCurriedFunc :: Int -> ([a] -> a) -> Value a
-- toCurriedFunc n f = curryTr n $ Transform $ \params -> do
--     args <- mapM eval params
--     let isOpaque (Opaque _) = True
--         isOpaque _ = False
--         fromOpaque (Opaque o) = o
--         fromOpaque _ = error "Not an opaque value"
--     if all isOpaque args
--         then pure . Opaque $ f (map fromOpaque args)
--         else throwError "Error"

-- mathTr :: Num n => (a -> n) -> (n -> a) -> (n -> n -> n) -> Value a
-- mathTr toNum fromNum f =
--     toCurriedFunc 2 $ \[x, y] -> fromNum $ f (toNum x) (toNum y)

-- eqTr :: Eq a => (Bool -> a) -> Value a
-- eqTr fromBool = toCurriedFunc 2 $ \[a, b] -> fromBool (a == b)

-- ifTr :: (a -> Bool) -> Value a
-- ifTr toBool = Transform $ \[cond, ifTrue, ifFalse] -> do
--     cond' <-
--         eval cond >>= 
--             \case
--                 Opaque x -> pure $ toBool x
--                 _ -> throwError $ "if: Cannot evaluate: " ++ show cond
--     eval (if cond' then ifTrue else ifFalse)

-- letTr :: Value a
-- letTr = Transform $ \[bindingList, body] -> do
--     env <- get
--     case bindingList of
--         List bindings ->
--             case bindings of
--                 [] -> eval body
--                 (List [name, value]:rest) -> do
--                     v <- eval value
--                     withEnv (extendEnv name v env) $
--                         callTransform letTr [List rest, body]
--                 _ -> throwError "let: Invalid binding in binding list"
--         _ -> throwError "let: Invalid binding list"

-- letrecTr :: Value a
-- letrecTr = Transform $ \[bindingList, body] -> do
--     case bindingList of
--         List bindings ->
--             case bindings of
--                 [] -> eval body
--                 (List [name, value]:rest) -> mdo
--                     env <- get
--                     -- let newEnv = extendEnv name (Promise newEnv value) env
--                     let newEnv = extendEnv name v env
--                     v <- withEnv newEnv $ eval value
--                     withEnv newEnv $ callTransform letrecTr [List rest, body]
--                 _ -> throwError "let: Invalid binding in binding list"
--         _ -> throwError "let: Invalid binding list"

-- -- let1Tr :: Value a
-- -- let1Tr = Transform $ \[name, value, body] -> do
-- --     tr <- callTransform lambdaTr [name, body]
-- --     callTransform tr [value]

-- -- letrec1Tr :: Value a
-- -- letrec1Tr = Transform $ \[name, value, body] -> mdo
-- --     modify (extendEnv name v)
-- --     v <- eval value
-- --     eval body

