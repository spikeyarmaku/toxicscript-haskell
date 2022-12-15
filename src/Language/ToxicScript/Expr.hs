module Language.ToxicScript.Expr
    ( Expr (..)
    , emptyExpr
    , serializeExpr
    , drawExpr
    , mkSymbol
    , mkList
    ) where

import Data.List    ( intercalate )

import qualified Data.Text as T
--

data Expr -- a tagless version of Data.SExpresso.SExp
    = Symbol T.Text
    | List [Expr]
    deriving (Eq, Ord)

instance Show Expr where
    show = serializeExpr

mkSymbol :: String -> Expr
mkSymbol = Symbol . T.pack

mkList :: [Expr] -> Expr
mkList = List

emptyExpr :: Expr
emptyExpr = List []

serializeExpr :: Expr -> String
serializeExpr (Symbol a) = T.unpack a
serializeExpr (List xs) = "(" ++ unwords (map serializeExpr xs) ++ ")"

drawExpr :: Expr -> String
drawExpr expr = intercalate "\n" $ drawExpr' (getHeight expr) expr

drawExpr' :: Int -> Expr -> [String]
drawExpr' height (Symbol a) =
    show a :
        concat (replicate (height - 1) [replicate (getWidth (Symbol a)) ' '])
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
getWidth (Symbol a) = length . show $ a
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
