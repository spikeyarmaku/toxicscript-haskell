module Language.ToxicScript.Parse
    ( parseExpr -- T.Text -> Interpreter Expression
    , parseExprs
    , test
    , getRational
    , getString
    ) where

import Data.Void                            ( Void )
import Control.Monad                        ( void )
import Data.SExpresso.Language.SchemeR5RS   ( stringParser, interTokenSpace )
import Data.Char                            ( isDigit )
import Data.Maybe                           ( fromMaybe )
import Numeric                              ( readFloat, readSigned )

import qualified Data.Text                  as T
import qualified Text.Megaparsec.Char       as C
import qualified Data.SExpresso.SExpr       as S

import Text.Megaparsec
import Data.SExpresso.Parse.Char
import Data.SExpresso.Parse.Generic

import Language.ToxicScript.Expr

type Parser = Parsec Void T.Text

parseExpr :: T.Text -> Either String Expr
parseExpr t =
    case runParser (decodeOne parser) "" t of
        Left e -> Left (errorBundlePretty e)
        Right expr -> Right $ sexprToExpr expr

parseExprs :: T.Text -> Either String [Expr]
parseExprs t =
    case runParser (decode parser) "" t of
        Left e -> Left (errorBundlePretty e)
        Right exprs -> Right $ map sexprToExpr exprs

parser :: SExprParser Parser () T.Text
parser = setSpace interTokenSpace $ plainSExprParser symbol

symbol :: Parser T.Text
symbol = try stringSymbol <|> identifier

-- Sexpresso's string parser looks for `\\` and `\"`, any other combination
-- starting with a backslash is illegal
stringSymbol :: Parser T.Text
stringSymbol = stringParser >>= \txt -> pure $ T.snoc (T.cons '"' txt) '"'

identifier :: Parser T.Text
identifier = do
    notFollowedBy illegalSymbol
    T.pack <$> someTill anySingle (lookAhead illegalSymbol)

illegalSymbol :: Parser ()
illegalSymbol = void $ oneOf (";()\" \t\n\r" :: String)

getRational :: T.Text -> Maybe Rational
getRational = parseMaybe rationalLit

-- TODO decide on escaping method (double quotes, backslash, etc.)
-- Unescape quotes
getString :: T.Text -> Maybe T.Text
getString text =
    case T.uncons text of
        Just ('"', x) ->
            case T.unsnoc x of
                Just (txt, '"') -> Just txt
                _ -> Nothing
        _ -> Nothing

rationalLit :: Parser Rational
rationalLit = do
    let digits = takeWhile1P (Just "digit") isDigit :: Parser T.Text
    sign <- optional (C.char '+' <|> C.char '-') :: Parser (Maybe Char)
    digits1 <- digits
    dot <- optional (C.char '.') :: Parser (Maybe Char)
    digits2 <- optional digits
    let signText = maybe (T.pack "") T.singleton sign
        dotText = maybe (T.pack "") T.singleton dot
        digits2Text = fromMaybe (T.pack "") digits2
    let numText = T.concat [signText, digits1, dotText, digits2Text]
    case readSigned readFloat (T.unpack numText) of
        ((n, []):_) ->
            case sign of
                Just '-' -> pure (-n)
                _ -> pure n
        _ ->
            error $ "PANIC!!! rationalLit: readFloat couldn't parse the number "
                ++ T.unpack numText

-- sexprToBinExpr ``        = error
-- sexprToBinExpr `()`      = Nil
-- sexprToBinExpr `(a)`     = Cons (Atom "a") Nil
-- sexprToBinExpr `(a ())`  = Cons (Atom "a") (Cons Nil Nil)
-- sexprToBinExpr `(a b)`   = Cons (Atom "a") (Cons (Atom "b") Nil)
-- sexprToBinExpr `(() ())` = Cons Nil (SCons SNil SNil)
sexprToExpr :: S.Sexp T.Text -> Expr
sexprToExpr (S.SList _ es) = List (map sexprToExpr es)
sexprToExpr (S.SAtom a) = Symbol a

testPrint :: String -> String
testPrint inp =
    case parseExpr (T.pack inp) of
        Left e -> "`" ++ inp ++ "` => " ++ e
        Right s -> "`" ++ inp ++ "` => " ++ show s

test :: IO ()
test = do
    putStrLn . testPrint $ ""
    putStrLn . testPrint $ "()"
    putStrLn . testPrint $ "a"
    putStrLn . testPrint $ "(a)"
    putStrLn . testPrint $ "(a b)"
    putStrLn . testPrint $ "((a) b)"
    putStrLn . testPrint $ "(a (b))"
    putStrLn . testPrint $ "(a ((b)))"
    putStrLn . testPrint $ "(() ())"
    putStrLn . testPrint $ "(+ 1 2)"
    putStrLn . testPrint $ "(* 2 (+ 3 4))"

-- `` => 1:1:
--   |
-- 1 | <empty line>
--   | ^
-- unexpected end of input
-- expecting "#f", "#t", '"', '(', '+', '-', ';', digit, end of line, space, or
-- tab

-- `()` => SNil
-- `a` => 1:2:
--   |
-- 1 | a
--   |  ^
-- unexpected end of input

-- `(a)` => SCons (SAtom a) SNil
-- `(a b)` => SCons (SAtom a) (SCons (SAtom b) SNil)
-- `((a) b)` => SCons (SCons (SAtom a) SNil) (SCons (SAtom b) SNil)
-- `(a (b))` => SCons (SAtom a) (SCons (SCons (SAtom b) SNil) SNil)
-- `(a ((b)))` => SCons (SAtom a) (SCons (SCons (SCons (SAtom b) SNil) SNil)
-- SNil)
-- `(() ())` => SCons SNil (SCons SNil SNil)
-- `(+ 1 2)` => SCons (SAtom +) (SCons (SAtom 1) (SCons (SAtom 2) SNil))
-- `(* 2 (+ 3 4))` => SCons (SAtom *) (SCons (SAtom 2) (SCons (SCons (SAtom +)
-- (SCons (SAtom 3) (SCons (SAtom 4) SNil))) SNil))