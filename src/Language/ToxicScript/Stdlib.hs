module Language.ToxicScript.Stdlib where

import qualified Data.Text  as T

import Language.ToxicScript.Eval
import Language.ToxicScript.Env
import Language.ToxicScript.Expr
import Language.ToxicScript.Parse

fromCode :: Env (Term a) -> String -> String -> (Expr, Term a)
fromCode env name t =
    case parseExpr . T.pack $ t of
        Left e -> error e
        Right v -> (mkSymbol name, evalExpr env v)

mkGlobalEnv
    :: (Num n, RealFrac n, Eq a)
    => (Rational -> a) -> (T.Text -> a) -- For parsing
    -> (a -> n) -> (n -> a)         -- to/from numerical types
    -> Env (Term a)
mkGlobalEnv fromRat fromText toNum fromNum =
    extendEnvMany (stringsAndNumbers (Val . fromRat) (Val . fromText))
        [ (mkSymbol "lambda",   lambdaV)
        , (mkSymbol "let",      letV)
        , (mkSymbol "lets",     letsV)
        , (mkSymbol "letrec",   letrecV)
        , (mkSymbol "letrecs",  letrecsV)

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

lambdaV :: Term a
lambdaV = Abs $ \_ name -> Abs $ \staticEnv body -> Abs $ \dynEnv value ->
    lamF staticEnv dynEnv name body value

lamF :: Env (Term a) -> Env (Term a) -> Expr -> Expr -> Expr -> Term a
lamF staticEnv dynEnv (Atom name) body value =
    evalExpr (extendEnv (Atom name) (evalExpr dynEnv value) staticEnv) body
lamF env _ x _ _ = evalExpr env x

letF :: Env (Term a) -> Env (Term a) -> Expr -> Expr -> Expr -> Term a
letF staticEnv dynEnv name value body = lamF staticEnv dynEnv name body value

letV :: Term a
letV =
    Abs $ \_ name ->
        Abs $ \staticEnv value ->
            Abs $ \dynEnv body -> letF staticEnv dynEnv name value body

letrecV :: Term a
letrecV =
    Abs $ \_ name ->
        Abs $ \_ value ->
            Abs $ \dynEnv body ->
                let newEnv = extendEnv name (evalExpr newEnv value) dynEnv
                in  evalExpr newEnv body     

letsV :: Term a
letsV =
    Abs $ \staticEnv defs ->
        case defs of
            List pairs ->
                Abs $ \_ body ->
                    let newEnv =
                            foldl (\env (List [name, value]) ->
                                        extendEnv name (evalExpr env value) env)
                                    staticEnv pairs
                    in  evalExpr newEnv body
            _ -> error "defs: incorrect binding list"

letrecsV :: Term a
letrecsV =
    Abs $ \_ defs ->
        case defs of
            List pairs ->
                Abs $ \dynEnv body ->
                    let newEnv =
                            foldl (\env (List [name, value]) ->
                                        extendEnv name (evalExpr env value) env)
                                    dynEnv pairs
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
