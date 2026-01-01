{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}

module NewDesign.Program
  where
  
data Constraint value choice =
   Move :: forall n (n:Nat). NTuple n (value,choice) -> Constraint value choice
   Stay :: forall n (n:Nat). NTuple n (value,choice) -> Constraint value choice

-- need order? or order is part of the struct?

data Program t value choice energy =
  Program
    { choices :: [choice]
    , struct :: t value
    , constraints :: [Constraint value choice -> energy]
    }
    
--NTupleFunction keeps the same
--Move and Stay have only compilation difference. Stay compiles to aa* + a*a (ZZ), while Move compiles to a + a* (X +- iY)

--example structure for fermion and anyon
--other than the graph datastructure, users need to support the path calculation from one place to the other
--users also need to support the approximate partition of pi/2, used in the phase exp(i theta), 
--i.e., what is the smallest value of theta, as appx = pi/2/theta
data OrderedGraph t index value =
  Graph { 
          graph :: t index value
          path :: index -> index -> [index]
          appx :: Nat -- number of partition in pi/2. fermion is 1, and anyon can be any
        }
