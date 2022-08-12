module Lists where
import Control.Monad.Random ( Rand, StdGen, evalRandIO, getRandom )
import Data.List ( permutations, sort, sortBy )

-- Problem 1
myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs
myLast _ = undefined

-- Problem 2
myButLast :: [a] -> a
myButLast [x, _] = x
myButLast (x:xs) = myButLast xs
myButLast _ = undefined

-- Problem 3
elementAt :: [a] -> Integer -> a
elementAt xs 1 = head xs
elementAt (x:xs) n = elementAt xs (n - 1)
elementAt _ _ = undefined

-- Problem 4
myLength :: [a] -> Integer
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- Problem 5
myReverse :: [a] -> [a]
myReverse = foldl (\x y -> y : x) []

-- Problem 6
isEqualList :: Eq a => [a] -> [a] -> Bool
isEqualList [] [] = True
isEqualList (x:xs) (y:ys) = x == y && isEqualList xs ys
isEqualList _ _ = False

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = isEqualList xs (reverse xs)

-- Problem 7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concatMap flatten xs

-- Problem 8
compress :: Eq a => [a] -> [a]
compress (x1:x2:xs)
    | x1 == x2 = compress (x1:xs)
    | otherwise = x1 : compress (x2:xs)
compress xs = xs

-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack xs@(x:_) = (\(as, bs) -> as : pack bs) $ span (==x) xs

-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack

-- Problem 11
data Element a = Single a
               | Multiple Int a
    deriving Show

encodeModified :: Eq a =>  [a] -> [Element a]
encodeModified = map
    (\(count, elem) ->
        if count == 1
            then Single elem
            else Multiple count elem)
    . encode

-- Problem 12
decodeModified :: [Element a] -> [a]
decodeModified = concatMap
    (\x -> case x of
        Multiple count elem -> replicate count elem
        Single elem -> [elem])

-- Problem 13
encodeDirect :: Eq a => [a] -> [Element a]
encodeDirect [] = []
encodeDirect xs@(x:_) =
    (\(as, bs) -> case () of
        _ | length as == 1 -> Single (head as)
          | otherwise -> Multiple (length as) (head as)
        : encodeDirect bs)
    $ span (==x) xs

-- Problem 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

-- Problem 15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = replicate n x ++ repli xs n

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = take (n - 1) xs ++ dropEvery (drop n xs) n

-- Problem 17
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split (x:xs) 1 = ([x], xs)
split (x:xs) n = (\(as, bs) -> (x : as, bs)) $ split xs (n - 1)

-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice xs i k = drop (i - 1) $ take k xs

-- Problem 19
rotate :: [a] -> Int -> [a]
rotate xs n
    | n >= 0 = drop m xs ++ take m xs
    | n < 0 = rotate xs m
    where m = n `mod` length xs
rotate _ n = undefined

-- Problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt k xs = (xs !! (k - 1), take (k - 1) xs ++ drop k xs)

-- Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt elem xs k = take (k - 1) xs ++ [elem] ++ drop (k - 1) xs

-- Problem 22
range :: Int -> Int -> [Int]
range i k = [i..k]

-- Problem 23
rng :: Rand StdGen Int
rng = getRandom

randomPermute :: [a] -> Rand StdGen [a]
randomPermute [] = return []
randomPermute xs = do
    ind <- (`mod` length xs) <$> rng
    let (num, xs') = removeAt (ind + 1) xs
    (:) <$> return num <*> randomPermute xs'

rnd_select :: Show a => [a] -> Int -> IO String
rnd_select xs n = show <$>
    (evalRandIO
    $ map (xs !!)
    . take n
    <$> randomPermute [0..(length xs - 1)])

-- Problem 24
diff_select :: Int -> Int -> IO [Int]
diff_select n m = evalRandIO $ take n <$> randomPermute [1..m]

-- Problem 25
rnd_permu :: [a] -> IO [a]
rnd_permu = evalRandIO . randomPermute

-- Problem 26
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = []
combinations 1 (x:xs) = [x] : combinations 1 xs
combinations n (x:xs) =
    map (x:) (combinations (n - 1) xs)
    ++ combinations n xs
combinations _ [] = []

-- Problem 27
splitBy :: [Int] -> [a] -> [[a]]
splitBy [] _ = []
splitBy _ [] = []
splitBy (size:rest) elements =
    take size elements : splitBy rest (drop size elements)

group :: Ord a => [Int] -> [a] -> [[[a]]]
group sizes elements
    | sum sizes /= length elements = undefined
    | otherwise =
        compress
        $ sort
        $ map (map sort . splitBy sizes)
        $ permutations elements

-- Problem 28
lsort :: Ord a => [[a]] -> [[a]]
lsort xs = map snd $ sort (zip (map length xs) xs)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter id . map f

lfsort :: Ord a => [[a]] -> [[a]]
lfsort xs =
    map snd
    $ sort
    $ zip (map (\x -> count (\y -> length y == length x) xs) xs) xs
