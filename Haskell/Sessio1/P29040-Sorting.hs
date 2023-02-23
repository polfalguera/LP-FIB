insert :: [Int] -> Int -> [Int]
insert [] n = [n]
insert xs n
    | (head xs >= n) = (n:xs)
    | otherwise = [head xs] ++ insert (tail xs) n 

isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert (isort xs) x

remove :: [Int] -> Int -> [Int]
remove [] n = []
remove xs n
    | (head xs == n) = tail xs
    | otherwise = [head xs] ++ remove (tail xs) n

ssort :: [Int] -> [Int]
ssort [] = []
ssort xs = min:(ssort (remove xs min))
    where 
        min = minimum xs

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y      = x : (merge xs (y:ys))
    | otherwise   = y : (merge (x:xs) ys)

-- Funciona però al jutge dona EE
--merge xs (y:ys) = merge xs' ys
--    where
--        xs' = insert xs y

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = merge (msort firstPart) (msort secondPart)
    -- Aixó també funciona, el EE el causava la funció merge comentada
    -- | (length xs == 2) = if (head xs) > (last xs) then reverse xs else xs
    -- | otherwise = merge (msort firstPart) (msort secondPart)
    where
        split = splitAt (div (length xs) 2) xs 
        firstPart = fst split 
        secondPart = snd split   

qsort :: [Int] -> [Int]
qsort [] = []
qsort (p:xs) = (qsort menors) ++ [p] ++ (qsort majors)
    where
        menors = [x | x <- xs, x <  p]
        majors = [x | x <- xs, x >= p]

genQsort :: Ord a => [a] -> [a]
genQsort [] = []
genQsort (p:xs) = (genQsort menors) ++ [p] ++ (genQsort majors)
    where
        menors = [x | x <- xs, x <  p]
        majors = [x | x <- xs, x >= p]