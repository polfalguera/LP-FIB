flatten :: [[Int]] -> [Int]
flatten xs = foldl (++) [] xs

myLength :: String -> Int
myLength "" = 0
myLength s = foldl1 (+) (map (const 1) s)

myReverse :: [Int] -> [Int]
myReverse xs = foldr (\x y -> y ++ [x]) [] xs 

countIn :: [[Int]] -> Int -> [Int]
countIn xs n = map length (map (filter (== n)) xs)

firstWord :: String -> String
firstWord s = takeWhile (/= ' ') s'
    where s' = dropWhile(== ' ') s

--firstWord :: String -> String
--firstWord s = takeWhile notSpace (dropWhile isSpace s)
--    where
--        isSpace :: Char -> Bool
--       isSpace c = (c == ' ')
--
--        notSpace:: Char -> Bool
--        notSpace c = (c /= ' ')
