-- ex. 1 Length function

rwhsLength :: [a] -> Int
rwhsLength (_:xs) = 1 + rwhsLength xs
rwhsLength []   = 0

-- ex 3. Mean function
rwhsMean a = (snd (inner a))/(fromIntegral (fst (inner a)))
         where inner [] = (0, 0)
               inner (a:as) = (1 + fst (inner as), a + snd (inner as))
