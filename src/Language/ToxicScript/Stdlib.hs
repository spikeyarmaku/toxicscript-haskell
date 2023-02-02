-- for letrec:
-- https://lptk.github.io/programming/2019/10/15/simple-essence-y-combinator.html

module Language.ToxicScript.Stdlib where

import qualified Data.Text  as T

import Data.Either  ( fromRight )

import Language.ToxicScript.Eval
import Language.ToxicScript.Env
import Language.ToxicScript.Expr
import Language.ToxicScript.Parse

fromCode :: Env (Term a) -> String -> String -> (Expr, Term a)
fromCode env name t =
    case parseExpr . T.pack $ t of
        Left e -> error e
        Right v -> (mkSymbol name, evalExpr env v)

-- * / + - e pi
-- < > <= >= = true false
-- lambda
-- let letrec
-- if not and or
-- list cons nth map length filter

mkGlobalEnv
    :: (Num n, RealFrac n, Eq a)
    => (Rational -> a) -> (T.Text -> a) -- For parsing
    -> (a -> n) -> (n -> a)             -- to/from numerical types
    -> Env (Term a)
mkGlobalEnv fromRat fromText toNum fromNum =
    extendEnvMany (stringsAndNumbers (Val . fromRat) (Val . fromText))
        [ (mkSymbol "lambda",   lambdaV)
        , (mkSymbol "let-many", letManyV)
        , (mkSymbol "let",      letV)

        , (mkSymbol "eq?",      eqVal)
        , (mkSymbol "true",     trueV)
        , (mkSymbol "false",    falseV)

        , (mkSymbol "+",        mathVal toNum fromNum (+))
        , (mkSymbol "*",        mathVal toNum fromNum (*))
        , (mkSymbol "-",        mathVal toNum fromNum (-))
        , (mkSymbol "/",        mathVal toNum fromNum (/))
        
        , (mkSymbol "pi",       Val $ fromNum 3.141592653589)
        , (mkSymbol "e",        Val $ fromNum 2.718281828459)

        , (mkSymbol "empty?",   isEmptyV)

        , (mkSymbol "list",     listV)
        -- , (mkSymbol "eval",     evalV)
        ]

withStdlib :: Expr -> Expr
withStdlib code = List [stdlibExpr, code]

stdlibExpr :: Expr
stdlibExpr = fromRight (mkSymbol "error") . parseExpr . T.pack $
    "(let-many\
        \(\
            \(first  (lambda x (lambda y x)))\
            \(second (lambda x (lambda y y)))\
            \(cons   (lambda x (lambda y (lambda f (f x y)))))\
            \(fst    (lambda p (p first)))\
            \(snd    (lambda p (p second)))\
            \(head fst)\
            \(tail snd)\
            \(if\
                \(lambda cond\
                    \(lambda ifTrue (lambda ifFalse (cond ifTrue ifFalse)))))\
            \(map (lambda fn (lambda lst ((empty? lst) lst (cons (fn (fst lst)) (map fn (tail lst)))))))\
            \(length (lambda lst ((empty? lst) 0 (+ 1 (length (tail lst))))))\
            \(nth (lambda n (lambda lst (((eq? n 0) (head lst) (nth (- n 1) (tail lst)))))))\
        \)\
    \)"

-- evalV :: Term a
-- evalV = Abs $ \env e -> evalExpr env e

-- The cool thing about this list definition is that we use "cons" without it
-- being defined beforehands. It shall be defined in the standard lib
-- (stdlib.txc in this case), and thanks to `let-many`'s late binding, it will
-- work. And it is statically scoped (check scoping.txc).
listV :: Term a
listV =
    Abs $ \env lst ->
        case lst of
            Atom _ -> evalExpr env $ List [mkSymbol "cons", lst, List []]
            List xs ->
                evalExpr env $
                    foldr (\x expr -> List [mkSymbol "cons", x, expr])
                        (List []) xs

isEmptyV :: Term a
isEmptyV =
    Abs $ \env lst ->
        case evalExpr env lst of
            Exp (List []) -> trueV
            _ -> falseV

lambdaV :: Term a
lambdaV = Abs $ \_ name -> Abs $ \staticEnv body -> Abs $ \dynEnv value ->
    lamF staticEnv dynEnv name body value

lamF :: Env (Term a) -> Env (Term a) -> Expr -> Expr -> Expr -> Term a
lamF staticEnv dynEnv name body value =
    evalExpr (extendEnv name (evalExpr dynEnv value) staticEnv) body

letF :: Env (Term a) -> Env (Term a) -> Expr -> Expr -> Expr -> Term a
letF staticEnv dynEnv name value body = lamF staticEnv dynEnv name body value

letV :: Term a
letV =
    Abs $ \_ name ->
        Abs $ \staticEnv value ->
            Abs $ \dynEnv body -> letF staticEnv dynEnv name value body

-- true = (lambda x (lambda y x))
trueV :: Term a
trueV = Abs $ \_ x -> Abs $ \env _ -> evalExpr env x

falseV :: Term a
falseV = Abs $ \_ _ -> Abs $ \env y -> evalExpr env y

zcombE :: Expr
zcombE =
    let commonTerm = "(lambda x (f (lambda v (x x v))))"
    in  fromRight (List []) . parseExpr . T.pack
            $ "(lambda f (" ++ commonTerm ++ " " ++ commonTerm ++ "))"

letManyV :: Term a
letManyV =
    Abs $ \staticEnv defs ->
        case defs of
            List pairs ->
                Abs $ \_ body ->
                    let expr =
                            foldr
                                (\(List [name, value]) b ->
                                    List [List [mkSymbol "let", name,
                                        List [zcombE,
                                            List [mkSymbol "lambda", name,
                                                    value]]], b])
                                body pairs
                    in  evalExpr staticEnv expr
            _ -> error "let-many: incorrect binding list"

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

eqVal :: Eq a => Term a
eqVal = Abs $ \_ e1 -> Abs $ \env e2 ->
    case evalExpr env e1 of
            Val x ->
                case evalExpr env e2 of
                    Val y -> if x == y then trueV else falseV
                    _ -> error "Not a value"
            _ -> error "Not a value"
