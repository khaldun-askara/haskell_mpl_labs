type IntList = [Int]
qsort :: IntList -> IntList
qsort [] = []
qsort (x:t) =
    (qsort [y|y<-t, y<=x])
    ++[x]
    ++ (qsort [y|y<-t, y>x])
    
data BinTree t =
    None
  | Node (t, BinTree t, BinTree t)
-- вариант для ленивых
--    deriving Show
    deriving Eq
leaf x = Node (x, None, None)
-- вариант для не ленивых
instance (Show t) => Show (BinTree t) where
    show None = ""
    show (Node(x, None, None)) = show x
    show (Node(x, lt, None)) = "(" ++ show lt ++ ")" ++ show x
    show (Node(x, None, rt)) = show x ++ "(" ++ show rt ++ ")"
    show (Node (x, lt, rt)) = "(" ++ show lt ++ ")" ++ show x ++ "(" ++ show rt ++ ")"

t1 = Node(5, Node(3, None, None), Node(8, Node(6, None, None), None))
t2 = Node(5, leaf 3, Node(8, leaf 6, None))
t3 = leaf 'x'

mysum x y = x+y

main =
  do
    putStrLn "Введите первое число:"
    x <- getLine
    putStrLn "Введите второе число:"
    y <- getLine
    putStr "Сумма этих чисел: "
    (putStrLn.show) (mysum (read x) (read y))