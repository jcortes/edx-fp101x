module Book where

import Prelude hiding (Maybe, Just, Nothing, Applicative, pure, (<*>), (<$>), sequenceA)

data Maybe a = Nothing | Just a
  deriving Show

instance Functor Maybe where
  -- fmap' :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing = Nothing
  fmap g (Just x) = Just (g x)

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where
  -- pure :: a -> Maybe a
  pure = Just
  -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  Nothing <*> _ = Nothing
  (Just g) <*> mx = fmap g mx

data CMaybe a = CNothing | CJust Int a
  deriving (Show)

instance Functor CMaybe where
  fmap f CNothing = CNothingalertasynotificaciones
  fmap f (CJust counter x) = CJust (counter+1) (f x)

instance Applicative [] where
  pure x = [x]  
  fs <*> xs = [f x | f <- fs, x <- xs]

(<$>) :: (Functor f) => (a -> b) -> f a -> f b  
f <$> x = fmap f x

instance Applicative IO where  
  pure = return
  a <*> b = do
    f <- a
    x <- b
    return (f x)

myAction :: IO String
myAction = (++) <$> getLine <*> getLine

main = do
  a <- (++) <$> getLine <*> getLine
  putStrLn $ "The two lines concatenated turn out to be: " ++ a

instance Applicative ((->) r) where
  pure x = (\_ -> x)
  f <*> g = \x -> f x (g x)

-- instance Applicative ZipList where
--   pure x = ZipList (repeat x)
--   ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)

-- getChars :: Int -> IO String
-- getChars 0 = return []
-- getChars n = pure (:) <*> getChar <*> getChars (n-1)

sequenceA :: Applicative f => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = pure (:) <*> x <*> sequenceA xs

getChars :: Int -> IO String
getChars n = sequenceA (replicate n getChar)
