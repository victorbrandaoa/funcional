xor :: Bool -> Bool -> Bool
xor a b = (not (a && b)) && (a || b)

impl :: Bool -> Bool -> Bool
impl a b = (not a) || b

equiv :: Bool -> Bool -> Bool
equiv a b = ((not a) || b) && (a || (not b))

pow :: Int -> Int -> Int
pow x y | y == 0 = 1
        | y == 1 = x
        | otherwise = x * (pow x (y - 1))

fatorial :: Int -> Int
fatorial x | x == 0 || x == 1 = 1
           | otherwise = x * (fatorial (x - 1))

getFactors :: Int -> Int -> [Int]
getFactors n f | n == f = [n]
               | mod n f == 0 = [f] ++ getFactors n (f + 1)
               | otherwise = getFactors n (f + 1)

isPrime :: Int -> Bool
isPrime x = (length (getFactors x 1)) == 2

mdc :: Int -> Int -> Int
mdc x y | y == 0 = x
        | otherwise = mdc y (mod x y)

mmc :: Int -> Int -> Int 
mmc x y = div (x * y) (mdc x y)

coprimo :: Int -> Int -> Bool
coprimo x y = (mdc x y) == 1

myLength :: (Eq a) => [a] -> Int
myLength (x:xs) | xs == [] = 1
                | otherwise = 1 + myLength xs

myReverse :: (Eq a) => [a] -> [a]
myReverse (x:xs) | xs == [] = [x]
                 | otherwise = myReverse xs ++ [x]

myTake :: Int -> [a] -> [a]
myTake k (x:xs) | k == 0 = []
                | otherwise = [x] ++ myTake (k - 1) xs

myDrop :: Int -> [a] -> [a]
myDrop k (x:xs) | k == 1 = xs
                | otherwise = myDrop (k - 1) xs

myMaximum :: (Ord a) => [a] -> a
myMaximum (x:xs) | xs == [] = x
                 | x >= n = x
                 | otherwise = n
                 where
                   n = myMaximum xs

myMinimum :: (Ord a) => [a] -> a
myMinimum (x:xs) | xs == [] = x
                 | x <= n = x
                 | otherwise = n
                 where
                   n = myMinimum xs

mySum :: (Eq a, Num a) => [a] -> a
mySum (x:xs) | xs == [] = x
             | otherwise = x + mySum xs

myProduct :: (Eq a, Num a) => [a] -> a
myProduct (x:xs) | xs == [] = x
                 | otherwise = x * myProduct xs

myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs) = x == e || myElem e xs

myRange :: (Eq a, Ord a, Enum a) => a -> a -> [a]
myRange start end | start > end = []
                  | start == end = [start]
                  | otherwise = [start] ++ myRange (succ start) end
