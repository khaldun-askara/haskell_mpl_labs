isPrime' n d =
    if d^2 > n then True
    else if mod n d == 0 then False
        else isPrime n (d+1)
        
isPrime n = isPrime' n 2