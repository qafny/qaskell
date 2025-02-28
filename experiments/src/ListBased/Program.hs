module ListBased.Program
  where

data Program t a b c =
  Program
    { choices :: [b]
    , struct :: t a
    , view :: Int
    , constraints :: [(a, b)] -> c
    }

