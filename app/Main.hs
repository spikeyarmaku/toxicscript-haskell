module Main where

import System.IO        ( readFile )
import Data.Maybe       ( mapMaybe )
import System.Directory ( listDirectory, withCurrentDirectory )

import qualified Data.Text  as T

import Control.Monad.Writer
import Control.Monad.Except

import Language.ToxicScript
import Language.ToxicScript.Stdlib

--

main :: IO ()
main = do
    evalFile "test/sandbox.txc"
    -- evalFile "test/stdlib.txc"
    -- files <- listDirectory "test"
    -- mapM_ (withCurrentDirectory "test" . evalFile) files

evalFile :: FilePath -> IO ()
evalFile fp = do
    expr <- readFile fp
    putStrLn $ "\n--- EVALUATING " ++ show fp ++ " ---\n"
    ast <- withHandler $ parseExpr (T.pack expr)
    -- putStrLn $ showExpr ast
    case evalExpr globalEnv (withStdlib ast) of
        Val x -> print x
        Var x -> putStrLn $ "Variable " ++ show x
        Abs _ -> putStrLn "Abstraction"

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

globalEnv :: Env (Term AppValue)
globalEnv = mkGlobalEnv VRat (VStr . T.unpack) avToNum VRat

avToBool :: AppValue -> Bool
avToBool (VBln b) = b
avToBool _ = error ""

avToNum :: AppValue -> Rational
avToNum (VRat r) = r
avToNum _ = error ""

data AppValue
    = VRat Rational
    | VStr String
    | VBln Bool
    deriving (Eq, Show)

