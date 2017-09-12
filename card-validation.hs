toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]

toDigits 401
toDigits (40) ++ [1]
toDigits (4) ++ [0]