module GeneralTraversable.Program
  where

data Program t a b c =
  Program
    { choices :: [b]
    , struct :: t a
    , view :: Int
    , constraints :: t (a, b) -> c
    }

