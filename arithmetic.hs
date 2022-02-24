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

