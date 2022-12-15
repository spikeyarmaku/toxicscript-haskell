{-# LANGUAGE LambdaCase #-}

module Main (main) where

import System.IO            ( readFile )
import Data.Maybe           ( mapMaybe )
import System.Environment   ( getArgs, getProgName )

import qualified Data.Text  as T

import Control.Monad.Writer
import Control.Monad.Except

import Language.ToxicScript
import Language.ToxicScript.Stdlib

--
main :: IO ()
main = getArgs >>=
    \case
    [] -> getProgName >>= putStrLn . helpMessage
    filepath:_ -> do
        expr <- readFile filepath
        ast <- withHandler $ parseExpr (T.pack expr)
        case runWriter (runExceptT (eval globalEnv ast)) of
            (Left e, logs) -> putStrLn $ "\nLOGS:\n" ++ logs ++ "\nERROR: " ++ e 
            (Right c, logs) -> do
                putStrLn $ "LOGS:\n" ++ logs
                case c of
                    Opaque  e -> print e
                    s -> print s

helpMessage :: String -> String
helpMessage cmdname = unlines
    [ cmdname ++ " reads a .txc file and interprets it."
    , "USAGE:"
    , cmdname ++ " filepath" ]

withHandler :: Show a => Either a b -> IO b
withHandler eith =
    case eith of
        Left e -> error $ show e
        Right r -> pure r

globalEnv :: Env (Value AppValue)
globalEnv =
    mkEnv (stringsAndNumbers ratToAppVal strToAppVal)
        [ mkTransform "lambda"  $ const lambdaTr
        , mkTransform "let"     $ const letTr
        , mkTransform "letrec"  $ const letrecTr
        , mkTransform "list"    $ const listTr
        , mkTransform "cons"    consTr
        , mkTransform "nth"     $ nthTr avToInt
        , mkTransform "if"      $ const $ ifTr avToBool
        , mkTransform "+"       $ const $ mathTr avToNum avFromNum (+)
        , mkTransform "*"       $ const $ mathTr avToNum avFromNum (*)
        , mkTransform "-"       $ const $ mathTr avToNum avFromNum (-)
        , mkTransform "/"       $ const $ mathTr avToNum avFromNum (/)
        , mkTransform "="       $ const $ eqTr avFromBool
        ]

strToAppVal :: T.Text -> Value AppValue
strToAppVal = Opaque . VStr . T.unpack

ratToAppVal :: Rational -> Value AppValue
ratToAppVal = Opaque . VRat

avToBool :: AppValue -> Bool
avToBool (VBln b) = b
avToBool _ = error ""

avFromBool :: Bool -> AppValue
avFromBool = VBln

avToNum :: AppValue -> Rational
avToNum (VRat r) = r
avToNum _ = error ""

avToInt :: AppValue -> Int
avToInt = round . avToNum

avFromNum :: Rational -> AppValue
avFromNum = VRat

data AppValue
    = VRat Rational
    | VStr String
    | VBln Bool
    deriving (Eq, Show)

