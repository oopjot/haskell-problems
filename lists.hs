-- Problem 1
myLast :: [a] -> a
myLast [a] = a
myLast (h : t) = myLast t
myLast [] = error "No end for empty lists!"

-- Problem 2
myButLast :: [a] -> a
myButLast (x : _ : []) = x
myButLast (h : t) = myButLast t
myButLast _ = error "List with at least 2 elems."


-- Problem 3
elementAt :: [a] -> Int -> a
elementAt [] _ = error "Empty list."
elementAt [a] 1 = a
elementAt (h : t) n
          | n - 1 == 0 = h
          | otherwise  = elementAt t (n-1)


-- Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (_ : t) = myLength t + 1

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [a] = [a]
myReverse (h : t) = myReverse t ++ [h]


-- Problem 6
isPalindrome :: (Ord a) => [a] -> Bool
isPalindrome [] = True
isPalindrome l = foldl (&&) True [f == s | (f, s) <- zip l (reverse l)]

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
myFlatten :: NestedList a -> [a]
myFlatten (List []) = []
myFlatten (Elem a) = [a]
myFlatten (List (h : t)) = myFlatten h ++ myFlatten (List t)

-- Problem 8
compress :: (Ord a) => [a] -> [a]
compress l = compress' l [] where
              compress' (h : t) [] = compress' t [h]
              compress' [] acc = reverse acc
              compress' (h : t) acc
                | h == (head acc) = compress' t acc
                | otherwise       = compress' t (h : acc)


-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = let (f, r) = span (==x) xs
              in (x : f) : pack r


-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = let (f, r) = span (==x) xs
                in (length (x : f), x) : encode r

