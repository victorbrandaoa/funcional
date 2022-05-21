penultimo [] = error "Lista sem penultimo"
penultimo (x:[]) = error "Lista sem penultimo"
penultimo (x:y:[]) = x
penultimo (x:xs) = penultimo xs

elementAt 1 (x:xs) = x
elementAt i (x:xs) = elementAt (i - 1) xs

isPalindrome [] = True
isPalindrome (x:[]) = True
isPalindrome xs | (last xs) /= (head xs) = False
                | otherwise = isPalindrome (tail (init xs)) 

compress [] = []
compress xs | elem (last xs) (init xs) = compress (init xs)
            | otherwise = (compress (init xs)) ++ [last xs]

removeElement xs e = filter (/=e) xs

compact [] = []
compact (x:xs) = (filter (==x) (x:xs)) ++ compact (removeElement xs x)

count xs e = length (filter (==e) xs)

encode [] = []
encode (x:xs) = (x, count (x:xs) x):encode (removeElement xs x)

split xs i = [take i xs] ++ [drop i xs]

sumFoldl (x:xs) = foldl (+) x xs

length' xs = foldl (\acc _ -> 1 + acc) 0 xs

mean xs = (fromIntegral $ sumFoldl xs) / (fromIntegral $ length' xs)

myAppend [] ys = ys
myAppend (x:xs) ys = x:(myAppend xs ys)

myAppend' xs ys = foldr (\x acc -> x:acc) ys xs

unique [] = []
unique (x:xs) = x:unique (removeElement xs x)

unique' xs = foldr (\x acc -> if elem x acc then acc else x:acc) [] xs

myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs

concatFoldr xs = foldr (++) [] xs

lessThan xs p = filter (<p) xs

greaterThan xs p = filter (>=p) xs

lessThan' xs p = [x | x <- xs, x < p]

greaterThan' xs p = [x | x <- xs, x >= p]

quickSortFilter [] = []
quickSortFilter (x:xs) = (quickSortFilter (lessThan xs x)) ++ [x] ++ (quickSortFilter (greaterThan xs x))

quickSort [] = []
quickSort (x:xs) = (quickSort (lessThan' xs x)) ++ [x] ++ (quickSort (greaterThan' xs x))

slice (x:xs) imin 1 = [x]
slice (x:xs) 1 imax = x:slice xs 1 (imax - 1)
slice (x:xs) imin imax = slice xs (imin - 1) (imax - 1)

insertAt el 1 xs = el:xs
insertAt el pos (x:xs) = x:insertAt el (pos -1) xs

maxList xs = foldr max (last xs) (init xs)

buildPalindrome [] = []
buildPalindrome (x:xs) = [x] ++ buildPalindrome xs ++ [x]

find p [] = error "Nenhum elemento satisfaz o predicado"
find p (x:xs) | p x = x
              | otherwise = find p xs
