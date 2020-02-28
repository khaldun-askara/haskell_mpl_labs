import Data.Char (ord, chr)
-- parser :: [String] -> ([String], Tree)

data Tree =
    Num Int
    | Add Tree Tree
    | Sub Tree Tree
    | Mult Tree Tree
    | Div Tree Tree
    | Pow Tree Tree
    --deriving Show
instance Show Tree where
    show (Num a) = "(" ++ (show a) ++ ")"
    show (Add tree1 tree2) = "(" ++ show tree1 ++ "+" ++ show tree2 ++ ")"
    show (Sub tree1 tree2) = "(" ++ show tree1 ++ "-" ++ show tree2 ++ ")"
    show (Mult tree1 tree2) = "(" ++ show tree1 ++ "*" ++ show tree2 ++ ")"
    show (Div tree1 tree2) = "(" ++ show tree1 ++ "/" ++ show tree2 ++ ")"
    show (Pow tree1 tree2) = "(" ++ show tree1 ++ "^" ++ show tree2 ++ ")"

-- digit = /[0-9]/
isDigit ch = ch >= '0' && ch <= '9'
toDigit ch = (ord ch) - (ord '0')

isNum (ch:_) = isDigit ch
toNum str = foldl (\acc ch -> acc * 10 + toDigit ch) 0 str
-- Num = /[0-9]+/
-- Num = digit {digit}
-- Num = digit Num | digit
num (part : parts) | isNum part = (parts, Num (toNum part))

-- addsub = addsub ('+'|'-') num | num
-- addsub = num {('+'|'-') num}

-- addsub = muldiv {('+'|'-') muldiv}
-- muldiv = pow {('*'|'/') pow}
-- pow = num ('^') pow | num

pow parts = 
    let (parts1, left) = num parts in
        case parts1 of
        ("^":parts2) -> let (parts3, right) = pow parts2 in
            (parts3, Pow left right)
        _ -> (parts1, left)

muldiv parts =
    let (parts1, left) = pow parts in
        loop left parts1 where
            loop left parts =
                case parts of
                ("*":parts1) -> let (parts2, right) = pow parts1 in
                    loop (Mult left right) parts2
                ("/":parts1) -> let (parts2, right) = pow parts1 in
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

-- parser :: [String] -> ([String], Maybe a)

number = (\parts -> case parts of
    (x:parts1) | isNum x -> (parts1, [toNum x])
    _ -> (parts, []))

-- char
char ch = (\parts -> case parts of
    (x:parts1) | x == ch -> (parts1, [ch])
    _ -> (parts, []))

-- succ
success parts parts1 = (length parts) /= (length parts1)

-- pand
pand f1 f2 = (\parts ->
    let (parts1, a) = f1 parts in
        if success parts parts1 then
            let (parts2, b) = f2 parts1 in
                if success parts1 parts2 then
                    (parts2, a ++ b)
                else
                    (parts, [])
        else
            (parts, []))

-- or
por f1 f2 = (\parts ->
    let (parts1, a) = f1 parts in
        if success parts parts1 then
            (parts1, a)
        else
            let (parts2, b) = f2 parts in
                if success parts parts2 then
                    (parts2, b)
                else
                    (parts, []))

action f1 act = (\parts ->
    let (parts1, a) = f1 parts in
        (parts1, act a))

-- pow = num:x ('^') pow:y -> Pow x y | num:x -> Num x 
pow1 = (number `pand` (char "^") `pand` pow1) `por` number


-- opt
-- many
-- 





