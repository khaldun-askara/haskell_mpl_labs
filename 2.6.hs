-- lastElem (h:[]) = h
-- lastElem (h:t) = lastElem t

-- withoutlastElem (h:[]) = []
-- withoutlastElem (h:t) = [h] ++ withoutlastElem t

-- selfDual:: [(Integer)]->Bool
-- selfDual [] = True
-- selfDual (h:t) =
    -- if t == [] then False
    -- else
        -- if h/=(lastElem t) then selfDual (withoutlastElem t)
        -- else False where
            -- lastElem (h:[]) = h
            -- lastElem (h:t) = lastElem t

            -- withoutlastElem (h:[]) = []
            -- withoutlastElem (h:t) = [h] ++ withoutlastElem t

--selfDual:: [(Integer)]->Bool
selfDual [] = True
selfDual ([h]) = False
selfDual (h:t) =
        if h/=(last t) then selfDual (init t)
        else False 
        
myFunc1 = [0,0,0,1,0]
myFunc2 = [0,0,0,0,0]
myFunc3 = [0]
myFunc4 = []
myFunc5 = [0,0,0,1,0,0]
myFunc6 = [0,0,0,1,1]
myFunc7 = [0,0,1,1]