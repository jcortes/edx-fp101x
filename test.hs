double x = x + x

quadruple
  x =
    double (double x)

factorial
  n =
    product [1..n]

average
  ns =
    sum ns `div` length ns

doubleSmallNumber
  x =
    if x > 100
    then x
    else x * 2

-- halve xs =
--   splitAt (length xs `div` 2) xs
-- halve xs =
--   (take (n `div` 2) xs, drop (n `div` 2) xs)
--   where n = length xs
-- halve xs = splitAt (div (length xs) 2) xs
halve xs = (take n xs, drop n xs)
  where n = length xs `div` 2

-- safetail (_:xs)
--   | null xs = []
--   | otherwise = tail xs

-- mult x y z = \ x -> (\ y -> (\ z -> x * y * z))
-- mult = \ x -> (x * \ y -> (\ z -> x * y * z))
mult = \ x -> (\ y -> (\ z -> x * y * z))

fix :: (a -> a) -> a
-- cb a = a
-- f cb = cb
fix f = let x = f x in x

