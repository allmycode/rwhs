-- ex. 1 Length function

rwhsLength :: [a] -> Int
rwhsLength (_:xs) = 1 + rwhsLength xs
rwhsLength []   = 0

-- ex 3. Mean function
rwhsMean a = (snd (inner a))/(fromIntegral (fst (inner a)))
         where inner [] = (0, 0)
               inner (a:as) = (1 + fst (inner as), a + snd (inner as))

-- ex 4/5. Turn list into palindrome.
-- Write function to check if list is palindrome

-- reverse is helper function for palindrome functions
rwhsReverse (x:xs) = rev' (x:[]) xs
  where rev' a (l:ls) = rev' (l:a) ls
        rev' a [] = a

rwhsMakePalindrome l = l ++ rwhsReverse l
                   
rwhsIsPalindrome l = l == rwhsReverse l
