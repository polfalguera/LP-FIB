myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f n [] = n
myFoldl f n (x:xs) = myFoldl f (f n x) xs 

myFoldr :: (a -> b -> b) -> b -> [a] -> b 
myFoldr f n [] = n
myFoldr f n (x:xs) = f x (myFoldr f n xs) 

myIterate :: (a -> a) -> a -> [a]
myIterate f n = n : (myIterate f (f n))

myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil f_bool f n 
    | (f_bool n) = n
    | otherwise  = myUntil f_bool f (f n)

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = [f x | x <- xs]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f_bool xs = [x | x <- xs,(f_bool x)]

myAll :: (a -> Bool) -> [a] -> Bool
myAll f_bool xs = and $ map f_bool xs
-- Alternativa que funciona perè en el Jutge dona EE
-- myAll f_bool xs = foldl (&&) True (map f_bool xs)

myAny :: (a -> Bool) -> [a] -> Bool
myAny f_bool xs = or $ map f_bool xs
-- Alternativa que funciona perè en el Jutge dona EE
-- myAny f_bool xs = foldl (||) False (map f_bool xs)

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f xs ys = [f x y | (x,y) <- myZip xs ys]