module Language.ToxicScript.Expr
    ( Expr (..)
    , Symbol (..)
    , emptyExpr
    , serializeExpr
    , showExpr
    , drawExpr
    , mkSymbol
    , mkList
    ) where

import Data.List    ( intercalate )

import qualified Data.Text as T
--

data Expr -- a tagless version of Data.SExpresso.SExp
    = Atom Symbol
    | List [Expr]
    deriving (Eq, Ord)

newtype Symbol = Symbol T.Text deriving (Eq, Ord)

instance Show Expr where
    show = serializeExpr

instance Show Symbol where
    show (Symbol s) = T.unpack s

mkSymbol :: String -> Expr
mkSymbol = Atom . Symbol . T.pack

mkList :: [Expr] -> Expr
mkList = List

emptyExpr :: Expr
emptyExpr = List []

serializeExpr :: Expr -> String
serializeExpr (Atom a) = show a
serializeExpr (List xs) = "(" ++ unwords (map serializeExpr xs) ++ ")"

showExpr :: Expr -> String
showExpr (Atom a) = "Atom " ++ show a
showExpr (List xs) = "List [" ++ intercalate ", " (map showExpr xs) ++ "]"

drawExpr :: Expr -> String
drawExpr expr = intercalate "\n" $ drawExpr' (getHeight expr) expr

drawExpr' :: Int -> Expr -> [String]
drawExpr' height (Atom a) =
    show a :
        concat (replicate (height - 1) [replicate (getWidth (Atom a)) ' '])
drawExpr' height (List []) =
    "█" : concat (replicate (height - 1)
            [replicate (getWidth (List [] :: Expr)) ' '])
drawExpr' height l@(List (x:xs)) =
    let rightStart = getWidth l - getWidth (List xs) -- 8
        -- lineLength = ((rightStart + getMid r - getMid l + 1) - 1) `div` 2
        leftLineLength = getMid l - getMid x
        rightLineLength = rightStart + getMid (List xs) - getMid l
        mainline =
            replicate (getMid x) ' '       ++ "┌" ++
            replicate (leftLineLength - 1) '─' ++ "┴" ++
            replicate (rightLineLength - 1) '─' ++ "┐" ++
            replicate (getWidth l - (getMid x + 1) - leftLineLength -
                        rightLineLength) ' '
    in  mainline :
            zipWith3 (\a b c -> a ++ b ++ c)
                (drawExpr' (height - 1) x)
                (replicate height
                    (replicate (rightStart - getWidth x) ' '))
                (drawExpr' (height - 1) (List xs))

getWidth :: Expr -> Int
getWidth (Atom a) = length . show $ a
getWidth (List (x:xs)) = max 3 $ getWidth x + getWidth (List xs) + 1
getWidth (List []) = length . drawExpr $ (List [] :: Expr)

getMid :: Expr -> Int
-- average of the two child mid-points
getMid (List (x:xs)) =
    let leftMid = getMid x
        rightMid = getWidth x + 1 + getMid (List xs)
    in  (leftMid + rightMid) `div` 2
getMid x = getWidth x `div` 2

getHeight :: Expr -> Int
getHeight (List (x:xs)) = length (x:xs) + maximum (map getHeight (x:xs))
getHeight _ = 1
