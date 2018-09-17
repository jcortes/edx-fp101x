n = a `div` length xs
  where a = 10
        xs = [1,2,3,4,5]

last xs = drop (length xs - 1) xs

myMap :: (a -> a) -> [a] -> [a]
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs

mySum xs = foldr (+) 0 xs