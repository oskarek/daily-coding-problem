module DailyCodingProblem.Year2019.June.June13.Solution where

cons :: a -> b -> ((a -> b -> c) -> c)
cons a b =
    let pair f = f a b
     in pair

car :: ((a -> b -> a) -> a) -> a
car f = f const

cdr :: ((a -> b -> b) -> b) -> b
cdr f = f (flip const)
