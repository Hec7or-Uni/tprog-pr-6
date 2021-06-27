module BinaryTree where
import Data.List

data Tree a = Empty
            | Leaf a
            | Node a (Tree a) (Tree a)

instance (Show a) => Show(Tree a) where
show t = show' t 0
    where
        show' :: Show a => Tree a -> Integer -> String
        show' Empty n = "<>"
        show' (Leaf x) n = show x
        show' (Node x lc rc) n = show x ++ "\n" ++ indent n ++ "|-" ++ show' lc (n+1) ++ "\n" ++ indent n ++ "|-" ++ show' rc (n+1)

        indent :: Integer -> String
        indent 0 = ""
        indent n = "  " ++ indent (n-1)

----- mascara -------------------------
empty :: a -> Tree a
empty _ = Empty

leaf :: a -> Tree a
leaf l = Leaf l

tree :: a -> Tree a -> Tree a -> Tree a
tree x lc rc = Node x lc rc

size :: Tree a -> Int
size (Empty)  = 0
size (Leaf l) = 1
size (Node a lc rc) = 1 + size lc + size rc

add :: Ord a => Tree a -> a -> Tree a
add Empty new = Leaf new
add (Leaf l) new
    | new <= l = Node l (Leaf new) (Empty)
    | new >  l = Node l (Empty) (Leaf new)
add (Node a lc rc) new
    | new <= a = Node a (add lc new) rc
    | new >  a = Node a lc (add rc new)

build :: Ord a => [a] -> Tree a
build []  = Empty
build [l] = Leaf l
build l   = build' Empty l
    where
        build' :: Ord a => Tree a -> [a] -> Tree a
        build' t (n:[]) = add (t) n
        build' t (n:ns) = build' (add (t) n)  ns

build2 :: Ord a => [a] -> Tree a
build2 []  = Empty
build2 [x] = Leaf x
build2 (x:xs) = Node x (build2 (filter (<= x) xs)) (build2 (filter (> x) xs))

buildBalanced :: (Ord a) => [a] -> Tree a
buildBalanced []  = Empty
buildBalanced [l] = Leaf l
buildBalanced l   = Node (med) (buildBalanced ll) (buildBalanced rl)
    where
        sz = length l
        med = mediana (sort l) sz
        ll  = fst (splitAt (sz `div` 2) l)
        rl' = snd (splitAt (sz `div` 2) l)
        rl  = Data.List.delete med rl'

        mediana :: [a] -> Int -> a
        mediana l sz = snd (splitAt (sz `div` 2) l) !! 0

preorder :: Ord a => Tree a -> [a]
preorder Empty = []
preorder (Leaf l) = [l]
preorder (Node a lc rc) = [a] ++ preorder lc ++ preorder rc

postorder :: Ord a => Tree a -> [a]
postorder Empty = []
postorder (Leaf l) = [l]
postorder (Node a lc rc) = postorder lc ++ postorder rc ++ [a]

inorder :: Ord a => Tree a -> [a]
inorder Empty = []
inorder (Leaf l) = [l]
inorder (Node a lc rc) = inorder lc ++ [a] ++ inorder rc

balance :: Ord a => Tree a -> Tree a
balance t = buildBalanced (balance' t)
    where
        balance' :: Ord a => Tree a -> [a]
        balance' a = inorder a

between :: Ord a => Tree a -> a -> a -> [a]
between Empty _ _ = []
between (Leaf l) xmin xmax
    | xmin <= l && l <= xmax = [l]
    | otherwise = []
between (Node a lc rc) xmin xmax
    | a < xmin  = between rc xmin xmax
    | a > xmax  = between lc xmin xmax
    | a <= xmin = between rc xmin xmax ++ [a]
    | a >= xmax = between lc xmin xmax ++ [a]
    | otherwise = between lc xmin xmax ++ [a] ++ between rc xmin xmax