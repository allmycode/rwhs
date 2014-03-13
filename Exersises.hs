-- ex. 1 Length function

rwhsLength :: [a] -> Int
rwhsLength (_:xs) = 1 + myLength xs
rwhsLength []   = 0
