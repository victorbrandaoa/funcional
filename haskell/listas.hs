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
