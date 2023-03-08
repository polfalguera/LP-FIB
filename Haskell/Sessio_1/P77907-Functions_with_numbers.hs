absValue :: Int -> Int
absValue n = if n < 0 then n*(-1) else n

power :: Int -> Int -> Int
power x 0 = 1
power x p = x * power x (p-1)

isPrime :: Int -> Bool
isPrime n
    | n <= 1 = False
    | otherwise = (length divisors) == 0 -- otherwise = res
    where
        divisors = [x | x <- [2..(floor (sqrt (fromIntegral n)))],mod n x == 0]
        -- res = null [x | x <- [2..(floor (sqrt (fromIntegral n)))],mod n x == 0]

slowFib :: Int -> Int
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib(n - 1) + slowFib(n - 2)

quickFib :: Int -> Int
quickFib 0 = 0
quickFib 1 = 1
quickFib n = auxFib 0 1 n

auxFib :: Int -> Int -> Int -> Int
auxFib n1 n2 0 = n1
auxFib n1 n2 iteration = auxFib n2 (n1 + n2) (iteration - 1) 


