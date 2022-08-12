module LogicCodes where
import Data.List (sort)
import Data.Tuple (swap)

-- Problem 46
and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

not' :: Bool -> Bool
not' True = False
not' False = True

nand' :: Bool -> Bool -> Bool
nand' x y = not' $ and' x y

nor' :: Bool -> Bool -> Bool
nor' x y = not' $ or' x y

xor' :: Bool -> Bool -> Bool
xor' x y = and' (or' x y ) (not $ and' x y)

impl' :: Bool -> Bool -> Bool
impl' x  = or' (not' x)

equ' :: Bool -> Bool -> Bool
equ' x y = not $ xor' x y

table :: (Bool -> Bool -> Bool) -> String
table f = unlines
    $ map
        (\(a, b) -> unwords $ map show [a, b, f a b])
        [(x, y) | x <- [True, False], y <- [True, False]]

-- Problem 47
infixl 1 `or'`
infixl 1 `nor'`
infixl 2 `xor'`
infixl 3 `and'`
infixl 3 `nand'`
infixl 4 `impl'`
infixl 4 `equ'`
infixl 5 `not'`

table2 :: (Bool -> Bool -> Bool) -> String
table2 = table

-- Problem 48
genTruthCombos :: Integer -> [[Bool]]
genTruthCombos 0 = [[]]
genTruthCombos n = map (True:) rest ++ map (False:) rest
    where rest = genTruthCombos (n - 1)

tablen :: Integer -> ([Bool] -> Bool) -> String
tablen n f = unlines
    $ map
        (\xs -> unwords $ map show (xs ++ [f xs]))
        (genTruthCombos n)

-- Problem 49
gray :: Integer -> [String]
gray 0 = [[]]
gray n = map ('0':) rest ++ map ('1':) (reverse rest)
    where rest = gray (n - 1)

-- Problem 50
data Tree a = Node { left :: Tree a, value :: a, right :: Tree a }
            | Leaf { value :: a }
    deriving Show

instance Eq a => Eq (Tree a) where
    lhs == rhs = value lhs == value rhs
               && left lhs == left rhs
               && right lhs == right lhs

instance Ord a => Ord (Tree a) where
    compare lhs rhs = compare (value lhs) (value rhs)

createHuffmanQueue :: [(Char, Integer)] -> [Tree (Integer, Char)]
createHuffmanQueue = map Leaf . sort . map swap

buildHuffmanTree :: [Tree (Integer, Char)] -> Tree (Integer, Char)
buildHuffmanTree [] = undefined
buildHuffmanTree [x] = x
buildHuffmanTree (x:y:rest) =
    buildHuffmanTree $ sort (newTree : rest)
        where (xn, _) = value x
              (yn, _) = value y
              xLess = Node x (xn + yn, '*') y
              yLess = Node y (xn + yn, '*') y
              newTree = if xn > yn then yLess else xLess

collapseHuffmanTree :: Tree (Integer, Char) -> [(Char, String)]
collapseHuffmanTree (Leaf (_, char)) = [(char, "")]
collapseHuffmanTree (Node left (_, '*') right) =
    map (\(x, y) -> (x, '0':y)) (collapseHuffmanTree left)
    ++ map (\(x, y) -> (x, '1':y)) (collapseHuffmanTree right)
collapseHuffmanTree _ = undefined

huffman :: [(Char, Integer)] -> [(Char, String)]
huffman = sort . collapseHuffmanTree . buildHuffmanTree . createHuffmanQueue
