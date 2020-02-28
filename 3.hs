import Data.Char (ord, chr)
import Control.Monad.State 

data Expr =
    Num Int
    | Add Expr Expr
    | Sub Expr Expr
    | Mult Expr Expr
    | Div Expr Expr

instance Show Expr where
    show (Num a) = "(" ++ (show a) ++ ")"
    show (Add expr1 expr2) = "(" ++ show expr1 ++ "+" ++ show expr2 ++ ")"
    show (Sub expr1 expr2) = "(" ++ show expr1 ++ "-" ++ show expr2 ++ ")"
    show (Mult expr1 expr2) = "(" ++ show expr1 ++ "*" ++ show expr2 ++ ")"
    show (Div expr1 expr2) = "(" ++ show expr1 ++ "/" ++ show expr2 ++ ")"
    
isDigit ch = ch >= '0' && ch <= '9'
toDigit ch = (ord ch) - (ord '0')

isNum (ch:_) = isDigit ch
toNum str = foldl (\acc ch -> acc * 10 + toDigit ch) 0 str

num (part : parts) | isNum part = (parts, Num (toNum part))

muldiv parts =
    let (parts1, left) = num parts in
        loop left parts1 where
            loop left parts =
                case parts of
                ("*":parts1) -> let (parts2, right) = num parts1 in
                    loop (Mult left right) parts2
                ("/":parts1) -> let (parts2, right) = num parts1 in
                    loop (Div left right) parts2
                _ -> (parts, left)

addsub parts =
    let (parts1, left) = muldiv parts in
        loop left parts1 where
            loop left parts =
                case parts of
                ("+":parts1) -> let (parts2, right) = muldiv parts1 in
                    loop (Add left right) parts2
                ("-":parts1) -> let (parts2, right) = muldiv parts1 in
                    loop (Sub left right) parts2
                _ -> (parts, left)
 
parse string = snd (addsub (words string)) 

rpn (Num a) = (show a) ++ " "
rpn (Add expr1 expr2) = (rpn expr1) ++ (rpn expr2) ++ "+" ++ " "
rpn (Sub expr1 expr2) = (rpn expr1) ++ (rpn expr2) ++ "-" ++ " "
rpn (Mult expr1 expr2) = (rpn expr1) ++ (rpn expr2) ++ "*" ++ " "
rpn (Div expr1 expr2) = (rpn expr1) ++ (rpn expr2) ++ "/" ++ " "


--подсчёт обратной польской записи
calcRPN = head . foldl foldingFunction [] . words  
    where   foldingFunction (x:y:ys) "*" = (x * y):ys  
            foldingFunction (x:y:ys) "/" = (y `div` x):ys
            foldingFunction (x:y:ys) "+" = (x + y):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys
            foldingFunction xs numberString = read numberString:xs  
            
calc = calcRPN . rpn . parse