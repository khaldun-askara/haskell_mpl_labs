-- combination:: Int -> Int -> Int
-- combination n m = 
    -- if m>n || m==0 && n==0 || m<0 || n<0 then 0
    -- else if m==0 || (m==n && n>0) then 1
    -- else сombination' n m 1 1 where
            -- сombination' n m i res =
                -- if i>m then res
                -- else (сombination' n m (i+1) (div (res*(n - m + i)) i))
                
-- combination:: Int -> Int -> Int
-- combination n m
       -- | m>n || m==0 && n==0 || m<0 || n<0 = 0
       -- | m==0 || (m==n && n>0) = 1
       -- | otherwise = сombination' n m 1 1 where
            -- сombination' n m i res =
                -- if i>m then res
                -- else (сombination' n m (i+1) (div (res*(n - m + i)) i))
                
combination:: Integer -> Integer -> Integer
combination 0 0 = 0
combination n m
       | m>n || m<0 || n<0 = 0
       | m==0 || (m==n && n>0) = 1
       | otherwise = div (factorial n) ((factorial m)*(factorial (n-m))) where
            factorial x = foldl (*) 1 [1..x]

            
-- combination:: Integer -> Integer -> Integer
-- combination 0 0 = 0
-- combination n 1 = n
-- combination _ 0 = 1
-- combination n m
       -- | m>n || m<0 || n<0 = 0
       -- | m==n && n>0 = 1
       -- | otherwise = div (factorial n) ((factorial m)*(factorial (n-m))) where
            -- factorial x = foldl (*) 1 [1..x]