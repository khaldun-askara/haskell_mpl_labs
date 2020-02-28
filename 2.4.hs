easyMonths weekends = easyMonth weekends [] where
    easyMonth [] count =
        map fst (filter (\(month, num) -> num == (maximum (map snd count))) count)
    easyMonth ((month, num):tail) count =
        if (elem month (map fst count)) then easyMonth tail count
        else easyMonth tail ((month, (length(filter (\x -> x == month) (map fst tail))+1)):count)     
        
myList = [("Jan", 10),("Jan", 4),("Feb", 1),("March", 3),("Apr", 1), ("Apr", 10)] 

