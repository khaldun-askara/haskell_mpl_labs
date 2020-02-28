-- minThree (n11, n12, w1) (n21, n22, w2) =
    -- if w2 < w1 then (n21, n22, w2)
    -- else (n11, n12, w1)
    

--minWay:: [([Char], [Char], Integer)] -> [([Char], [Char], Integer)]
minWay:: [([Char], [Char], Integer)] -> [([Char], [Char])]
minWay listOfCity =
    map (\(n1,n2,w) -> (n1, n2)) (filter (\(n1, n2, w) -> (w == (justWay (foldl1 minThree listOfCity)))) listOfCity) where
    justWay (n1, n2, w) = w
    minThree (n11, n12, w1) (n21, n22, w2) =
        if w2 < w1 then (n21, n22, w2)
        else (n11, n12, w1)

myList = [("1", "2", 10),("3", "4", 9),("5", "6", 7), ("7", "8", 7)]