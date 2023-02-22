absValue :: Int -> Int
absValue n = if n < 0 then n*(-1) else n

power :: Int -> Int -> Int
power x 0 = 1
power x p = x * power x (p-1)

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True

slowFib :: Int -> Int
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib(n - 1) + slowFib(n - 2)

quickFib :: Int -> Int
