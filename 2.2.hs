-- onlySecond [] = []
-- onlySecond ((mfst, msecd):tail) = [msecd] ++ onlySecond tail

findAllSon listOfSons name = onlySecond (ffindAllSon listOfSons listOfSons name) where
    ffindAllSon [] allList name = []
    ffindAllSon ((father, son):t) allList name = if father == name then
        [(father, son)] ++ (ffindAllSon allList allList son) ++ (ffindAllSon t allList name)
        else (ffindAllSon t allList name)
    onlySecond [] = []
    onlySecond ((mfst, msecd):tail) = [msecd] ++ onlySecond tail
            

myList = [("firstdad", "firstson"),
        ("firstson", "secondson"),
        ("firstson", "thirdson"),
        ("secondson", "fourthson"),
        ("thirdson", "fiftson"),
        ("fourthson", "sixthson"),
        ("fourthson", "seventhson"),
        ("eigthson", "ninthson"),
        ("eigthson", "tenthson"),
        ("fiftson", "eigthson")]