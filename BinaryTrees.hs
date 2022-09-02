module BinaryTrees where
import Data.List ( sort )
import Lists ( compress )

data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Branch x Empty Empty

-- Problem 55
saturated :: Tree a -> Bool
saturated Empty = True
saturated (Branch _ Empty Empty) = True
saturated (Branch _ Empty _) = False
saturated (Branch _ _ Empty) = False
saturated (Branch _ left right) = saturated left && saturated right

count :: Tree a -> Integer
count Empty = 0
count (Branch _ left right) = count left + height right + 1

insertBy :: a -> (Tree a -> Integer) -> Tree a -> [Tree a]
insertBy x f Empty = [leaf x]
insertBy x f (Branch val Empty Empty) = concatMap
    (\tree -> [Branch val tree Empty, Branch val Empty tree])
    (insertBy x f Empty)
insertBy x f (Branch val Empty right) = map
    (\tree -> Branch val tree right)
    (insertBy x f Empty)
insertBy x f (Branch val left Empty) = map
    (Branch val left)
    (insertBy x f Empty)
insertBy x f (Branch val left right)
    | (f left < f right) || not (saturated left) =
        map (\tree -> Branch val tree right) (insertBy x f left)
    | (f right < f left) || not (saturated right) =
        map (Branch val left) (insertBy x f right)
    | otherwise =
        map (\tree -> Branch val tree right) (insertBy x f left)
        ++ map (Branch val left) (insertBy x f right)

instance Ord a => Ord (Tree a) where
    Empty <= _ = True
    _ <= Empty = False
    (Branch xVal xLeft xRight) <= (Branch yVal yLeft yRight)
        | compare xVal yVal /= GT = True
        | compare xLeft yLeft /= GT = True
        | otherwise = compare xRight yRight /= GT

cbalTree :: Integer -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = compress
    $ sort
    $ concatMap (insertBy 'x' count) (cbalTree (n - 1))

-- Problem 56
xify :: Tree a -> Tree Char
xify Empty = Empty
xify (Branch val left right) =
    Branch 'x' (xify left) (xify right)

invert :: Tree a -> Tree Char
invert Empty = Empty
invert (Branch val left right) =
    Branch 'x' (invert right) (invert left)

symmetric :: Eq a => Tree a -> Bool
symmetric Empty = True
symmetric tree =
    xify tree == invert tree

-- Problem 57
add :: Ord a => Tree a -> a -> Tree a
add Empty x = leaf x
add (Branch val left right) x
    | x > val = Branch val left (add right x)
    | otherwise = Branch val (add left x) right

construct :: Ord a => [a] -> Tree a
construct = foldl add Empty

-- Problem 58
symCbalTrees :: Integer -> [Tree Char]
symCbalTrees = filter symmetric . cbalTree

-- Problem 59
height :: Tree a -> Integer
height Empty = 0
height (Branch _ left right) = max (height left) (height right) + 1

balTree :: Ord a => a -> Integer -> [Tree a]
balTree val 0 = [Empty]
balTree val n = compress
    $ sort
    $ concatMap (insertBy val count) (balTree val (n - 1))

hbalTree :: Ord a => a -> Integer -> [Tree a]
hbalTree _ 0 = [Empty]
hbalTree val n = concatMap (balTree val) [2^(n - 1)..2^n - 1]

-- Problem 60
minNodes :: Integer -> Integer
minNodes h
    | h < 1 = 0
    | otherwise = 1 + minNodes (h - 1) + minNodes(h - 2)

binSearch :: (Integer -> Bool) -> Integer -> Integer -> Integer
binSearch lessThanEq l r
    | l >= r = l - 1
    | lessThanEq mid = binSearch lessThanEq (mid + 1) r
    | otherwise = binSearch lessThanEq l mid
    where mid = l + (r - l) `div` 2

maxHeight :: Integer -> Integer
maxHeight n = binSearch (\h -> minNodes h <= n) 0 n

hbalTreeNodes :: Ord a => a -> Integer -> [Tree a]
hbalTreeNodes val n = compress $ sort $ concatMap (hbalTree val) [floor (logBase 2 (fromIntegral n)) + 1..maxHeight n]
