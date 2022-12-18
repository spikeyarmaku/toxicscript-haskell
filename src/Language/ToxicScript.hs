module Language.ToxicScript
    ( module Language.ToxicScript.Combination
    , module Language.ToxicScript.Parse
    , module Language.ToxicScript.Expr
    , module Language.ToxicScript.Eval
    , module Language.ToxicScript.Env
    ) where

import Language.ToxicScript.Combination
    ( getCombiner
    , getParams
    , isValidCombination
    , getCombination
    )
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
    ( Env
    , emptyEnv
    , stringsAndNumbers
    , extendEnv
    , extendEnvMany
    , lookupEnv
    )

import Language.ToxicScript.Expr

-- TODO write mkSyntax and mkFunction
-- TODO write tests