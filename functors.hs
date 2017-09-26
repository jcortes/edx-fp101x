import Prelude hiding (Functor, Maybe, Just, Nothing)

module Book where

inc :: [Int] -> [Int]
inc [] = []
inc (n:ns) = n + 1 : inc ns

sqr :: [Int] -> [Int]
sqr [] = []
sqr (n:ns) = n ^ 2 : sqr ns

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

inc' = map' (+1)
sqr' = map' (^2)

class Functor f where
  fmap' :: (a -> b) -> f a -> f b

-- instance Functor [] where
--   -- fmap' :: (a -> b) -> [a] -> [b]
--   fmap' = map'

instance Functor [] where
  -- fmap' :: (a -> b) -> f a -> f b
  fmap' g [] = []
  fmap' g (x:xs) = fmap' g xs ++ [g x]

data Maybe' a = Nothing | Just a
  deriving Show

instance Functor Maybe' where
  -- fmap' :: (a -> b) -> Maybe a -> Maybe b
  fmap' _ Nothing = Nothing
  fmap' g (Just x) = Just (g x)

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

instance Functor Tree where
  -- fmap' :: (a -> b) -> Tree a -> Tree b
  fmap' g (Leaf x) = Leaf (g x)
  fmap' g (Node l r) = Node (fmap' g l) (fmap' g r)

instance Functor IO where
  -- fmap' :: (a -> b) -> IO a -> IO b
  fmap' g mx = do { x <- mx; return (g x) }

inc'' :: Functor f => f Int -> f Int
inc'' = fmap' (+1)