-- module Book where

import Prelude hiding (Maybe, Just, Nothing)

data Maybe a = Nothing | Just a
  deriving Show

instance Functor Maybe where
  -- fmap' :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing = Nothing
  fmap g (Just x) = Just (g x)

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

instance Prelude.Applicative Maybe where
  -- pure :: a -> Maybe a
  pure = Just
  -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  Nothing <*> _ = Nothing
  (Just g) <*> mx = fmap g mx

data CMaybe a = CNothing | CJust Int a
  deriving (Show)

instance Functor CMaybe where  
  fmap f CNothing = CNothing
  fmap f (CJust counter x) = CJust (counter+1) (f x)

