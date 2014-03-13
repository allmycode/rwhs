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

-- ex 9-12. Using the code from the preceding three exercises,
-- implement Graham's scan algorithm

-- Point data type
data Point = Point Double Double
             deriving (Show)
x (Point x _) = x
y (Point _ y) = y

-- data Direction
data Direction = LeftTurn
                 | RightTurn
                 | Straight
                 deriving (Show, Eq)

-- calculate turn when traverse three points
getTurn (Point x1 y1) (Point x2 y2) (Point x3 y3)
  | dotProduct == 0 = Straight
  | dotProduct > 0 = LeftTurn
  | otherwise = RightTurn
  where dotProduct = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)

-- ex 11.
getTurns :: [Point] -> [Direction]
getTurns (a:b:c:ps) = (getTurn a b c) : (getTurns (b:c:ps))
getTurns _ = []

grahamScan ps = gs [] points
                where
                  (start, rest) = lowestPoint (head ps) [] (tail ps)
                    where
                      lowestPoint p r (p':ps) = if (y p') < (y p) then (lowestPoint p' (p:r) ps) else (lowestPoint p (p':r) ps) 
                      lowestPoint p r [] = (p, r)
                  points = start:(sortByPolarAngle start rest)
                    where
                      polarAngle (Point x y) (Point x' y')= (atan2 (x'-x) (y'-y))
                      polarComparer a b c = compare (polarAngle a c) (polarAngle a b) 
                      sortByPolarAngle p ps = sortBy (polarComparer p) ps
                  gs (b:a:ch) (c:ps)
                    | getTurn a b c == RightTurn = gs (a:ch) (c:ps)
                    | otherwise = gs (c:b:a:ch) ps
                  gs ch [] = ch
                  gs ch (p:ps) = gs (p:ch) ps 

-- test data
ps = [(Point 7 1), (Point 6 4), (Point 5 2),  (Point 4 3), (Point 3 0.5), (Point 2 4), (Point 4 7)]
ps2 = [(Point 5 5), (Point 3 2), (Point 5 4),  (Point 2 3), (Point 8 2), (Point 6 2), (Point 8 5), (Point 7 6), (Point 3 6), (Point 6 8)]
