import Data.Char

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

halve :: Ord a => [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs
-- halve xs =
--   (take (n `div` 2) xs, drop (n `div` 2) xs)
--   where n = length xs
-- halve xs = splitAt (div (length xs) 2) xs
-- halve xs = (take n xs, drop n xs)
--   where n = length xs `div` 2

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

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], isPerfect x]
  where isPerfect num = sum (init (factors num)) == num

find :: (Eq a) => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..n])
  where n = length xs - 1

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- xs `zip` ys]


let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

let2intUpper :: Char -> Int
let2intUpper c = ord c - ord 'A'

int2letUpper :: Int -> Char
int2letUpper n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | isUpper c = int2letUpper ((let2intUpper c + n) `mod` 26)
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

riffle :: [a] -> [a] -> [a]
riffle xs ys = concat [[x, y] | (x, y) <- xs `zip` ys]

divides :: Int -> Int -> Bool
divides n d = n `mod` d == 0

divisors :: Int -> [Int]
divisors x = [d | d <- [1..x], x `divides` d]


qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) =
  qsort smaller ++ [x] ++ qsort greater
  where
    smaller = [a | a <- xs, a <= x]
    greater = [b | b <- xs, b > x]

replicates :: Int -> a -> [a]
replicates 0 _ = []
replicates n a = a : replicates (n - 1) a

selectNthElement :: [a] -> Int -> a
selectNthElement (h:xs) 0 = h
selectNthElement (h:xs) n = selectNthElement xs (n-1)

element :: Eq a => a -> [a] -> Bool
element _ [] = False
element a (h:xs) =
  if h == a
  then True
  else element a xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) =
  if x <= y then x : merge xs (y:ys)
  else y : merge (x:xs) ys

-- merge (4:[5,6]) (1:[2,3])
-- (1 : merge (4:[5,6]) (2:[3]))
-- (1 : 2 : merge (4:[5,6]) (3:[]))
-- (1 : 2 : 3 : merge (4:[5,6]) [])
-- (1 : 2 : 3 : [4,5,6])

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
  where (ys, zs) = halve xs

-- msort [4,5,6,1,2,3]
-- merge (msort [4,5,6]) (msort [1,2,3])
-- merge (merge (msort [4]) (msort [5,6])) (merge msort([1]) msort( [2,3]))
-- merge (merge [4] (merge msort([5]) msort([6]))) (merge [1] (merge msort([2]) msort([3])) )
-- merge (merge [4] (merge [5] [6]) (merge [1] (merge [2] [3]) )
-- merge (merge [4] [5,6]) (merge [1] [2,3] )
-- merge [4,5,6] [1,2,3]
-- [1,2,3,4,5,6]


