module Percentage ( mkPercent) where

data Percent a = Percent a

mkPercent :: (Num a, Show a) => a -> Percent a
mkPercent = Percent

instance (Show a) => Show (Percent a) where
  show (Percent x) = show x ++ "%"

instance (Read a) => Read (Percent a) where
  

