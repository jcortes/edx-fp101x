toDigits :: Integer -> [Integer]
toDigits 0 = [0]
toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]

-- toDigits 1234
-- (++) (toDigits (1234 `div` 10))  [1234 `mod` 10]
-- (++) ((++) (toDigits (123 `div` 10)) [123 `mod` 10]) [1234 `mod` 10]
-- (++) ((++) ((++) (toDigits (12 `div` 10)) [12 `mod` 10]) [123 `mod` 10]) [1234 `mod` 10]
-- (++) ((++) ((++) ((++) (toDigits (1 `div` 10)) [1 `mod` 10]) [12 `mod` 10]) [123 `mod` 10]) [1234 `mod` 10]
-- (++) ((++) ((++) ((++) (toDigits (0)) [1]) [2]) [3]) [4]
-- (++) ((++) ((++) ((++) [0] [1]) [2]) [3]) [4]
-- (++) ((++) ((++) [0,1] [2]) [3]) [4]
-- (++) ((++) [0,1,2] [3]) [4]
-- (++) [0,1,2,3] [4]
-- [0,1,2,3,4]

toDigitsReverse :: Integer -> [Integer]
toDigitsReverse 0 = [0]
toDigitsReverse n = n `mod` 10 : toDigitsReverse (n `div` 10)

-- toDigitsReverse 1234
-- (:) (1234 `mod` 10) (toDigitsReverse (1234 `div` 10))
-- (:) (1234 `mod` 10) ((:) (123 `mod` 10) (toDigitsReverse (123 `div` 10)))
-- (:) (1234 `mod` 10) ((:) (123 `mod` 10) ((:) (12 `mod` 10) (toDigitsReverse (12 `div` 10))))
-- (:) (1234 `mod` 10) ((:) (123 `mod` 10) ((:) (12 `mod` 10) ((:) (1 `mod` 10) (toDigitsReverse (1 `div` 10)))))
-- (:) (4) ((:) (3) ((:) (2) ((:) (1) (toDigitsReverse (0)))))
-- (:) (4) ((:) (3) ((:) (2) ((:) (1) [0])))
-- (:) (4) ((:) (3) ((:) (2) [1,0]))
-- (:) (4) ((:) (3) [2,1,0])
-- (:) (4) [3,2,1,0]
-- [4,3,2,1,0]

doubleSecond :: [Integer] -> [Integer]
doubleSecond xs = [if even idx then y + y else y | (y,idx) <- ys]
  where ys = zip xs [1..(length xs)]

sumDigits :: [Integer] -> Integer
sumDigits xs = sum [if x < 10 then x else sum (toDigits x) | x <- xs]

isValid :: Integer -> Bool
isValid x = x `mod` 10 == 0

cardIsValid :: Integer -> Bool
cardIsValid n = 
  isValid (sumDigits (doubleSecond (toDigitsReverse n)))

eval xs = foldl (\x y -> y + (10 * x)) 0 xs
evalReverse xs = foldl (\x y -> x + (10 * y)) 0 xs