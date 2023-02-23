myLength :: [Int] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myMaximum :: [Int] -> Int
myMaximum (x:xs) = computeMax xs x

computeMax :: [Int] -> Int -> Int
computeMax [] max = max
computeMax (x:xs) max = if x > max then computeMax xs x else computeMax xs max

average :: [Int] -> Float
average xs = fromIntegral(sum xs) / fromIntegral(length xs)

buildPalindrome :: [Int] -> [Int]
buildPalindrome xs = (myReverse xs) ++ xs

myReverse :: [Int] -> [Int]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = (myReverse xs) ++ [x] 

inside :: Int -> [Int] -> Bool
inside x [y] = (x == y)
inside x (y:ys) = (x == y) || inside x ys

remove :: [Int] -> [Int] -> [Int]
remove [] [] = []
remove [] y = []
remove xs [] = xs
remove (x:xs) y
    | inside x y = remove xs y
    | otherwise = [x] ++ (remove xs y)

flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (x:xs) = x ++ (flatten xs)

oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens [] = ([],[])
oddsNevens (x:xs) 
    | even x    = (senars,x:parells) 
    | otherwise = (x:senars,parells)
    where
        (senars,parells) = oddsNevens xs

isPrime :: Int -> Bool
isPrime n
    | n <= 1 = False
    | otherwise = (length divisors) == 0 -- otherwise = res
    where
        divisors = [x | x <- [2..(floor (sqrt (fromIntegral n)))],mod n x == 0]
        -- res = null [x | x <- [2..(floor (sqrt (fromIntegral n)))],mod n x == 0]

primeDivisors :: Int -> [Int]
primeDivisors n = [x | x <- [2..n], (mod n x == 0) && isPrime(x)]

--primeDivisors :: Int -> [Int]
--primeDivisors n 
--    | isPrime n = []
--    | otherwise = primeDivisors' n 2
--
--primeDivisors' :: Int -> Int -> [Int]
--primeDivisors' n x
--    | (n < x) = []
--    | (mod n x == 0) && (isPrime x) = (x:primeDivisors' n (x+1))
--    | otherwise = primeDivisors' n (x+1)