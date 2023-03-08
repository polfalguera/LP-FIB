ones :: [Integer]
ones = repeat 1 -- or cycle [1] or iterate id 1 or 1 : ones

nats :: [Integer]
nats = iterate (+1) 0

ints :: [Integer]
ints = tail $ concat $ map (\x -> [x, -x]) nats

triangulars :: [Integer]
triangulars = tail $ scanl (+) 0 nats

factorials :: [Integer]
factorials = scanl (*) 1 (tail nats)

--Solució classe
fibs :: [Integer]
fibs = 0 : 1 : (zipWith (+) fibs (tail fibs))

--primes :: [Integer]
--primes = filter isPrime nats

--isPrime :: Integer -> Bool
--isPrime n
--    | n <= 1 = False
--    | otherwise = (length divisors) == 0 -- otherwise = res
--    where
--        divisors = [x | x <- [2..(floor (sqrt (fromIntegral n)))],mod n x == 0]

primes :: [Integer]
primes = garbell (tail $ tail nats)
    where
        garbell (p:xs) = p : garbell [x | x <- xs, x `mod` p /= 0]

hammings :: [Integer]
hammings = 1 : merge (map (*2) hammings) (map (*3) hammings) (map (*5) hammings)

merge :: [Integer] -> [Integer] -> [Integer] -> [Integer]
merge xs ys zs = merge2 (merge2 xs ys) zs

merge2 :: [Integer] -> [Integer] -> [Integer]
merge2 (x:xs) (y:ys) 
    | x == y    = x : merge2 xs ys
    | x < y     = x : merge2 xs (y:ys)
    | otherwise = y : merge2 (x:xs) ys

lookNsay :: [Integer]
lookNsay = iterate count 1
    where
        count :: Integer -> Integer
        count x
            | penultim == 0     = 10 + x
            | ultim == penultim = recursive + 10
            | otherwise         = recursive * 100 + 10 + ultim
            where
                ultim = mod x 10
                penultim = div (mod x 100) 10
                recursive = count(div x 10) 

tartaglia :: [[Integer]]
tartaglia = [1] : map nextRow tartaglia
    where
        nextRow r = 1 : zipWith (+) r (tail r) ++ [1]