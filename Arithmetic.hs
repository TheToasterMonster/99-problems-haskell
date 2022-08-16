module Arithmetic where
import Lists ( encode )

-- Problem 31
isPrime :: Integer -> Bool
isPrime x
    | x <= 1 = undefined
    | otherwise = all ((/=0) . (x `mod`)) [2..(x `div` 2)]

-- Problem 32
myGCD :: Integer -> Integer -> Integer
myGCD x y
    | x < 0 || y < 0 = myGCD (abs x) (abs y)
    | y == 0 = x
    | otherwise = myGCD y (x `mod` y)

-- Problem 33
coprime :: Integer -> Integer -> Bool
coprime x y = gcd x y == 1

-- Problem 34
totient :: Integer -> Integer
totient x = fromIntegral
    $ length
    $ filter id
    $ map (coprime x)
    $ [1..x - 1]

-- Problem 35
primeFactors :: Integer -> [Integer]
primeFactors x
    | isPrime x = [x]
    | otherwise = factor : primeFactors (x `div` factor)
        where factor = last (takeWhile isNotValidFactor [1..]) + 1
              isNotValidFactor y = rem y /= 0 || y == 1
              rem = (x `mod`)

-- Problem 36
prime_factors_mult :: Integer -> [(Integer, Int)]
prime_factors_mult x = map (\(x, y) -> (y, x)) $ encode $ primeFactors x

-- Problem 37
totient' :: Integer -> Integer
totient' = foldl (\x (y, p) -> x * f y p) 1 . prime_factors_mult
    where f y p = (y - 1) * y ^ (p - 1)

-- Problem 38
testTotient :: Integer
testTotient = totient 6969420

testTotient' :: Integer
testTotient' = totient' 6969420

-- Problem 39
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map
    (\x -> 2 * x + 1)
    (filter
        (\y -> y `notElem` map
            (\(a, b) -> a + b + 2 * a * b)
            (cartProd [1..n `div` 2] [1..n `div` 2]))
        [1..n])

primesR :: Integer -> Integer -> [Integer]
primesR lower upper = filter (\x -> x >= lower && x <= upper)
    $ 2 : sieveSundaram (upper `div` 2)

-- Problem 40
goldbach :: Integer -> (Integer, Integer)
goldbach x
    | odd x = undefined
    | otherwise = (ans, x - ans)
        where try = \(y:ys) ->
                if isPrime y && isPrime (x - y)
                    then y
                    else try ys
              ans = try [2..]

-- Problem 41
goldbachList :: Integer -> Integer -> [(Integer, Integer)]
goldbachList lower upper = map goldbach $ filter even $ [lower..upper]

goldbachList' :: Integer -> Integer -> Integer -> [(Integer, Integer)]
goldbachList' lower upper atLeast =
    filter (\(x, y) -> x > atLeast && y > atLeast)
    $ goldbachList lower upper
