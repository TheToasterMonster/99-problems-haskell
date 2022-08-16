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

height :: Tree a -> Integer
height Empty = 0
height (Branch _ left right) = max (height left) (height right) + 1

insert :: a -> Tree a -> [Tree a]
insert x Empty = [leaf x]
insert x (Branch val Empty Empty) = concatMap
    (\tree -> [Branch val tree Empty, Branch val Empty tree])
    (insert x Empty)
insert x (Branch val Empty right) = map
    (\tree -> Branch val tree right)
    (insert x Empty)
insert x (Branch val left Empty) = map
    (Branch val left)
    (insert x Empty)
insert x (Branch val left right)
    | (height left < height right) || not (saturated left) =
        map (\tree -> Branch val tree right) (insert x left)
    | (height right < height left) || not (saturated right) =
        map (Branch val left) (insert x right)
    | otherwise =
        map (\tree -> Branch val tree right) (insert x left)
        ++ map (Branch val left) (insert x right)

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
    $ concatMap (insert 'x') (cbalTree (n - 1))

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
