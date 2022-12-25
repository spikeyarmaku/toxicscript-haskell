-- left-associative
-- S-expression based
-- curried by default
-- strict (lazy evaluation is harder to implement)
-- functional
-- referentially transparent
-- dynamically typed
-- statically scoped
-- no reserved keywords

-- reason for generating meaning from symbol name:
    -- can write a hexadecimal / binary reader
    -- can generate cad*r combiners

module Language.ToxicScript
    ( module Language.ToxicScript.Combination
    , module Language.ToxicScript.Env
    , module Language.ToxicScript.Eval
    , module Language.ToxicScript.Expr
    , module Language.ToxicScript.Parse
    ) where

import Language.ToxicScript.Combination
import Language.ToxicScript.Env
import Language.ToxicScript.Eval
import Language.ToxicScript.Expr
import Language.ToxicScript.Parse
