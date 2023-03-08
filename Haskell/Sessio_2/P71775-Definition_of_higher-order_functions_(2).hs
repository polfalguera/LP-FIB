countIf :: (Int -> Bool) -> [Int] -> Int
countIf f xs = sum(map myFunction xs)
    where
        myFunction x 
            | f x = 1
            | otherwise = 0

-- REDUCED VERSION --
--countIf :: (Int -> Bool) -> [Int] -> Int
--countIf f xs = length(filter f xs)


-- RECURSIVE VERSION --
--countIf :: (Int -> Bool) -> [Int] -> Int
--countIf f xs = countIf' f xs 0
--
--countIf' :: (Int -> Bool) -> [Int] -> Int -> Int
--countIf' f [] i = i
--countIf' f (x:xs) i
--    | (f x) = (countIf' f xs i+1)
--    | otherwise = (countIf' f xs i)

pam :: [Int] -> [Int -> Int] -> [[Int]]
pam xs fs = map (`map` xs) fs

-- RECURSIVE VERSION --
--pam _ [] = []
--pam [] _ = []
--pam xs (f:fs) = [(map f xs)] ++ (pam xs fs)

pam2 :: [Int] -> [Int -> Int] -> [[Int]]
pam2 xs fs = map (\x -> map ($ x) fs) xs 

filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterFoldl f_bool f n xs = foldl f n (filter f_bool xs)

insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int]
insert f_bool xs n = takeWhile (`f_bool` n) xs ++ n:dropWhile (`f_bool` n) xs

insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int]
insertionSort f_bool xs = foldl (insert f_bool) [] xs -- insert f_bool xs([]) n(xs[1..length(xs)])