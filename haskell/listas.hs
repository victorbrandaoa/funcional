penultimo :: (Eq a, Ord a) => [a] -> a
penultimo xs | length xs <= 1 = error "Lista sem penultimo"
             | length (tail xs) == 1 = head xs
             | otherwise = penultimo (tail xs)

elementAt :: Int -> [a] -> a
elementAt i xs | i == 1 = head xs
               | otherwise = elementAt (i - 1) (tail xs)

isPalindrome :: (Eq a, Ord a) => [a] -> Bool
isPalindrome xs | length xs <= 1 = True
                | (last xs) /= (head xs) = False
                | otherwise = isPalindrome (tail (init xs))

compress :: (Eq a) => [a] -> [a]
compress xs | xs == [] = []
            | elem (last xs) (init xs) = compress (init xs)
            | otherwise = (compress (init xs)) ++ [last xs]

slice :: [a] -> Int -> Int -> [a]
slice xs imin imax | imax == 1 = [head xs]
                   | imin == 1 = [head xs] ++ slice (tail xs) imin (imax - 1)
                   | otherwise = slice (tail xs) (imin - 1) (imax - 1)

insertAt :: a -> Int -> [a] -> [a]
insertAt el pos xs | pos == 1 = [el] ++ xs
                   | otherwise = [head xs] ++ insertAt el (pos - 1) (tail xs)

maxList :: (Ord a) => [a] -> a
maxList xs = foldr max (last xs) (init xs)

buildPalindrome :: (Eq a) => [a] -> [a]
buildPalindrome xs | xs == [] = []
                   | otherwise = [head xs] ++ buildPalindrome (tail xs) ++ [head xs]

find :: (Eq a) => (a -> Bool) -> [a] -> a
find p xs | xs == [] = error "Nenhum elemento satisfaz o predicado"
          | p (head xs) = head xs
          | otherwise = find p (tail xs)

count :: (Eq a) => a -> [a] -> Int
count x [] = 0
count x xs | x == head xs = 1 + count x (tail xs)
           | otherwise = count x (tail xs)