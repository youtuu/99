-- 1
myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs
-- 2
myButLast :: [a] -> a
myButLast [x,y] = x
myButLast (_:xs) = myButLast xs
-- 3
elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt [] _ = error "hoge"
elementAt (_:xs) n
  | n < 1 = error "piyo"
  | otherwise = elementAt xs (n-1)
-- 4
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs
--5
myReverse :: [a] -> [a]
myReverse x = foldl (\acc x -> x : acc) [] x
--6
isPalindrome ::  (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)
--8
compress :: String -> String
compress [] = []
compress (x:xs) = foldl (\acc x -> f acc x) [x] xs
  where f acc x = if x `elem` acc then acc else acc ++ [x]
--9
simpleString :: String -> String
simpleString [] = []
simpleString [x] = [x]
simpleString arr = reverse $ foldl (\acc n -> if n `elem` acc then acc else n:acc) [] arr

singleGroup :: Char -> String -> String
singleGroup _ []  = []
singleGroup a str = foldl (\acc n -> if n == a then n:acc else acc) [] str

pack :: String -> [String]
pack [] = []
pack [x] = [[x]]
pack arr = foldl (\acc n -> acc ++ [singleGroup n arr]) [] $ simpleString arr
-10
search :: Char -> String -> Int
search _ [] = 0
search x y = length $ filter (==x) y

encode :: String -> [(Int, Char)]
encode a = zip (len a) $ simpleString a
  where len x  = map (\n -> search n x) $ simpleString x
