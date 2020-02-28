my_gcd:: Int -> Int -> Int
my_gcd n m | n < 0 || m <0 = 0
           | n/=0 && m /=0 =
                if n>m then my_gcd (mod n m) m
                else my_gcd n (mod m n)
           | otherwise = n+m