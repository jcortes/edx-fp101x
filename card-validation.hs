toDigits :: Integer -> [Integer]
toDigits 0 = [0]
toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsReverse :: Integer -> [Integer]
toDigitsReverse 0 = [0]
toDigitsReverse n = n `mod` 10 : toDigitsReverse (n `div` 10)

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