eql :: [Int] -> [Int] -> Bool
eql xs ys
    | (length xs) /= (length ys) = False
    | otherwise = all (== 0) (zipWith (-) xs ys)

prod :: [Int] -> Int 
prod [] = 1
prod xs = foldl1 (*) xs

prodOfEvens :: [Int] -> Int
prodOfEvens xs = foldl (*) 1 (filter even xs)

powersOf2 :: [Int]
powersOf2 = iterate (*2) 1

scalarProduct :: [Float] -> [Float] -> Float
scalarProduct xs ys = foldl1 (+) (zipWith (*) xs ys)

