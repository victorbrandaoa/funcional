data Quadruple a b = QVazio | Quadruple a a b b

firstTwo QVazio = Nothing
firstTwo (Quadruple a1 a2 b1 b2) =  Just (a1, a2)

secondTwo QVazio = Nothing
secondTwo (Quadruple a1 a2 b1 b2) = Just (b1, b2)

data Tuple a b c d = Tuple1 a | Tuple2 a b | Tuple3 a b c | Tuple4 a b c d

tuple1 (Tuple1 a) = Just a
tuple1 (Tuple2 a _) = Just a
tuple1 (Tuple3 a _ _) = Just a
tuple1 (Tuple4 a _ _ _) = Just a

tuple2 (Tuple2 _ b) = Just b
tuple2 (Tuple3 _ b _) = Just b
tuple2 (Tuple4 _ b _ _) = Just b
tuple2 _ = Nothing

tuple3 (Tuple3 _ _ c) = Just c
tuple3 (Tuple4 _ _ c _) = Just c
tuple3 _ = Nothing

tuple4 (Tuple4 _ _ _ d) = Just d
tuple4 _ = Nothing

data List a = Nil | Cons a (List a) deriving (Eq, Show)

listLength Nil = 0
listLength (Cons x xs) = 1 + listLength xs

listHead Nil = error "Empty list"
listHead (Cons x xs) = x

listTail Nil = error "Empty list"
listTail (Cons x xs) = xs

listFoldr f v Nil = v
listFoldr f v (Cons x xs) = f x (listFoldr f v xs)

listFoldl f v Nil = v
listFoldl f v (Cons x xs) = listFoldr f (f v x) xs

data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a) deriving (Eq, Show)

sizeBT NIL = 0
sizeBT (Node a left right) = 1 + sizeBT left + sizeBT right

maximumBT (Node a NIL NIL) = a
maximumBT (Node a left NIL) = max a (maximumBT left)
maximumBT (Node a NIL right) = max a (maximumBT right)
maximumBT (Node a left right) = max a (max (maximumBT left) (maximumBT right))

minimumBT (Node a NIL NIL) = a
minimumBT (Node a left NIL) = min a (minimumBT left)
minimumBT (Node a NIL right) = min a (minimumBT right)
minimumBT (Node a left right) = min a (min (minimumBT left) (minimumBT right))

isBST NIL = False
isBST (Node a NIL NIL) = True
isBST (Node a left right) | a > (maximumBT left) || a > (minimumBT right) = False
                          | otherwise = True

insertBST NIL v = (Node v NIL NIL)
insertBST (Node a left right) v | a > v = (Node a (insertBST left v) right)
                                | a < v = (Node a left (insertBST right v))
                                | otherwise = (Node a left right)

getNodeValue (Node a _ _) = a

getLeftBST (Node a left _) = left

getRightBST (Node a _ right) = right

searchBST NIL _ = NIL
searchBST (Node a left right) e | a == e = (Node a left right)
                                | e > a = searchBST right e
                                | e < a = searchBST left e

-- predecessor (Node a left right) e = maximumBT (getLeftBST subBST)
--                                     where
--                                       subBST = searchBST (Node a left right) e

-- sucessor (Node a left right) e = minimumBT (getRightBST subBST)
--                                  where
--                                    subBST = searchBST (Node a left right) e

predecessor = undefined
sucessor = undefined
remove = undefined

order (Node a NIL NIL) = [a]
order (Node a NIL right) = [a] ++ order right
order (Node a left NIL) = order left ++ [a]
order (Node a left right) = order left ++ [a] ++ order right

preorder (Node a NIL NIL) = [a]
preorder (Node a NIL right) = [a] ++ preorder right
preorder (Node a left NIL) = [a] ++ preorder left
preorder (Node a left right) = [a] ++ preorder left ++ preorder right

postorder (Node a NIL NIL) = [a]
postorder (Node a NIL right) = postorder right ++ [a]
postorder (Node a left NIL) = postorder left ++ [a]
postorder (Node a left right) = postorder left ++ postorder right ++ [a]
