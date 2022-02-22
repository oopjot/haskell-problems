-- Problem 1
myLast :: [a] -> a
myLast [a] = a
myLast (h : t) = myLast t
myLast [] = error "No end for empty lists!"

-- Problem 2
myButLast :: [a] -> a
myButLast [x, _] = x
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
                | h == head acc = compress' t acc
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


-- Problem 11
data EncodedItem a = Single a | Multiple Int a
                        deriving (Show)
encodeModified :: Eq a => [a] -> [EncodedItem a]
encodeModified l = map helper (encode l) where
                    helper (1, a) = Single a
                    helper (n, a) = Multiple n a 

-- Problem 12
decodeModified :: [EncodedItem a] -> [a]
decodeModified = concatMap decodeItem where
                    decodeItem (Single v) = [v]
                    decodeItem (Multiple n v) = replicate n v 


-- Problem 13
-- I don't understand how is that different from 11


-- Problem 14
dupli :: [a] -> [a]
dupli [] = []
dupli [x] = [x, x]
dupli (x : xs) = x : x : dupli xs


-- Problem 15
repli :: [a] -> Int -> [a]
repli l n = concatMap (replicate n) l


-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery l n = [x | (x, i) <- zip l [1 .. length l], mod i n /= 0]


-- Problem 17
split :: [a] -> Int -> ([a], [a])
split l n = foldl split' ([], []) l where
                split' (f, s) e
                    | length f < n = (reverse (e : reverse f), s)
                    | otherwise = (f, reverse (e : reverse s))


-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice l m n = [x | (x, i) <- zip l [1 .. n], i >= m]


-- Problem 19
rotate :: [a] -> Int -> [a]
rotate l n = if n < 0
                then
                    let l' = reverse l
                        n' = abs n
                    in reverse (drop n' l' ++ take n' l')
                else drop n l ++ take n l


-- Problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt 0 _ = error "Invalid index"
removeAt _ [] = error "Cannot remove from empty list"
removeAt n l = removeAt' l [] 1 where
                    removeAt' (x:xs) acc i
                        | i == n = (x, reverse acc ++ xs)
                        | otherwise = removeAt' xs (x : acc) (i + 1)
                    removeAt' [] _ _ = error "Unknown error"

