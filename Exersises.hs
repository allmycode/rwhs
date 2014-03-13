import Data.List (sortBy)

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

-- ex 6. Create a function that sorts a list of lists based on the
-- length of each sublist
rwhsListLengthComparator a b = compare (length a) (length b) 
rwhsSortLists ll = sortBy rwhsListLengthComparator ll

-- ex 7. Function that joins a list of lists together using a separator
-- value
rwhsIntersperse s (l:ls) = l ++ intersperse' s ls
  where intersperse' s (l:ls) =  s : l ++ intersperse' s ls
        intersperse' s [] = []
rwhsIntersperse s [] = []

-- ex 8. Write a function that will determine the height of the tree
data BinaryTree a = Node a (BinaryTree a) (BinaryTree a)
            | Empty
            deriving (Show)

rwhsTreeHeight (Node _ l r) = 1 + (max (rwhsTreeHeight l) (rwhsTreeHeight r))
rwhsTreeHeight Empty = 0

-- test data
b0 = Empty
b1 = Node 1 Empty Empty
b2 = Node 1 b1 Empty
b32 = Node 1 b2 b1

