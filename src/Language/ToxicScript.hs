-- left-associative
-- S-expression based
-- curried by default
-- lazy evaluation
-- functional
-- referentially transparent
-- dynamically typed
-- statically scoped
-- no reserved keywords

-- fexpr-based (like kernel)? or use haskell-like metaprogramming?
    -- Probably metaprogramming would make more sense in a scripting language
-- based on lambda calculus?

-- https://crypto.stanford.edu/~blynn/lambda/
-- https://github.com/blynn/compiler/blob/master/lambda.lhs
-- http://www.dalnefre.com/wp/2011/11/fexpr-the-ultimate-lambda/
-- http://ympbyc.github.io/blog/curried-s-expression.clj.html
-- https://www.cs.princeton.edu/~dpw/cos441-11/notes/slides15-lambda-proofs.pdf
-- https://hugopeters.me/posts/16/
-- https://bor0.wordpress.com/2019/03/19/writing-a-lambda-calculus-evaluator-in-haskell/
-- https://jameshfisher.com/2018/03/15/a-lambda-calculus-interpreter-in-haskell/
-- https://sookocheff.com/post/fp/representing-pairs-and-lists-in-lambda-calculus/
-- https://github.com/AngelAlvie/LambdaLight
-- http://lambda-the-ultimate.org/node/4346
-- https://www.schoolofhaskell.com/user/bss/magma-tree

-- introspective lambda
-- http://homepage.divms.uiowa.edu/~astump/papers/archon.pdf
-- https://www.reddit.com/r/types/comments/73rwob/introspective_lambda_calculus/
-- https://sci-hub.se/https://doi.org/10.1145/2103746.2103765

-- https://dumas.ccsd.cnrs.fr/dumas-00530710/document
-- http://pauillac.inria.fr/~levy/pubs/90popljfp.pdf
-- https://hal.archives-ouvertes.fr/hal-00384683/document
-- https://www.irif.fr/~kesner/papers/springer-csl07.pdf
-- https://github.com/quchen/stgi
-- https://sci-hub.se/10.1007/3-540-15975-4_50
-- https://github.com/andrejbauer/plzoo/blob/master/src/lambda/syntax.ml

-- reason for generating meaning from symbol name:
-- can write a hexadecimal / binary reader
-- can generate cad*r combiners

module Language.ToxicScript
    ( module Language.ToxicScript.Combination
    , module Language.ToxicScript.Parse
    , module Language.ToxicScript.Expr
    , module Language.ToxicScript.Eval
    , module Language.ToxicScript.Env
    ) where

-- TODO write export lists
import Language.ToxicScript.Combination
--     ( getCombiner
--     , getParams
--     , isValidCombination
--     , getCombination
--     )

import Language.ToxicScript.Parse
    ( parseExpr
    , parseExprs
    , getRational
    , getString
    )

import Language.ToxicScript.Eval
    -- ( Value (..)
    -- , Result (..)
    -- , Eval
    -- , eval
    -- , callTransform
    -- , addValue
    -- , runEval
    -- )

import Language.ToxicScript.Env
--     ( Env
--     , emptyEnv
--     , stringsAndNumbers
--     , extendEnv
--     , extendEnvMany
--     , lookupEnv
--     )

import Language.ToxicScript.Expr

-- TODO write tests