module Constraint where

{--

[1,2,3]

=> 

[ [1,2,3], [1,2,-3], [1,-2,3], [1,-2,-3], [-1,2,3], [-1,2,-3], [-1,-2,3], [-1,-2,-3] ]

=>

[ 36, 0, 4, 16, 16, 4, 0, 36 ]

Solutions: ++- or --+

--}

data Sg a = Plus a | Minus a deriving Show

generate :: [a] -> [ [Sg a] ]
generate = mapM (\a -> [ Plus a, Minus a ]) 

sg :: Sg Int -> Int
sg (Plus a) = a
sg (Minus a) = -a

energy :: [ [Sg Int] ] -> [ ([Sg Int], Int) ]
energy = map (\ss -> (ss, sum (map sg ss)))

ground :: [ ([Sg Int], Int) ] -> [ ([Sg Int], Int) ] 
ground = filter (\(ss,e) -> e == 0)

solve :: [Int] -> [ ([Sg Int], Int) ]
solve = ground . energy . generate

{--

ghci> solve [1,2,3]
[([Plus 1,Plus 2,Minus 3],0),([Minus 1,Minus 2,Plus 3],0)]


--}
