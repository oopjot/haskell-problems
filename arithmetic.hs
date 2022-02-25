-- Problem 31
isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = and [n `mod` i /= 0 | i <- [3..n-1]]

-- Problem 32
myGcd :: Int -> Int -> Int
myGcd a 0 = a
myGcd a b = myGcd b (a `mod` b)


-- Problem 33
coprime :: Int -> Int -> Bool
coprime a b = myGcd a b == 1


-- Problem 34
totient :: Int -> Int
totient n = length [x | x <- [1..n], coprime n x]


-- Problem 35
primeFactors :: Int -> [Int]
primeFactors n
    | isPrime n = [n]
    | otherwise = pf : primeFactors (n `div` pf) where
        pf = head [x | x <- [2..n], isPrime x, n `mod` x == 0]

-- Problem 36
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult n = foldr f [] (primeFactors n) where
                    f el ((e, i):xs)
                        | e == el = (e, i + 1) : xs
                        | otherwise = (el, 1) : (e, i) : xs
                    f el [] = [(el, 1)]


-- Problem 37
phi :: Int -> Int
phi n = product [(p - 1) * p ^ (m - 1) | (p, m) <- primeFactorsMult n]


-- Problem 39
primesR :: Int -> Int -> [Int]
primesR n m = [x | x <- [n..m], isPrime x]


-- Problem 40
goldbach :: Int -> (Int, Int)
goldbach n = head [(x, y) | x <- primes, y <- primes, x + y == n]
                where primes = primesR 2 n

                    
-- Problem 41
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList n m = [goldbach x | x <- [n..m], even x]

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' n m k = filter (\(a, b) -> a >= k && b >= k) (goldbachList n m)

