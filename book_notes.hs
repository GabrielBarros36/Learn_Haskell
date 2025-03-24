absolute num =
    if num > 0 
    then num
    else num * (-1)

inc num = 
    num + 1

double :: Num a => a -> a
double num = 
    num * 2

square num =
    num * num

weird n =
    if mod n 2 == 0
        then n-2
    else
        3*n + 1

-- Rewrite this using a lambda function instead of where
doubleDouble :: Num a => a -> a
doubleDouble x =
    dubs * 2
    where dubs = x * 2

doubleDoubleLambda x =
    (\num -> num * 2) x * 2

-- Usually more elegant to just use let instead of where/lambda
doubleDoubleLet x =
    let dub = x * 2 -- variables defined
        sample = x * 3 -- unused, but showcasing syntax
    in -- body begins
     dub * 2 -- single-spaced indent

-- Rewrite this using only lambdas
overwrite x =
    let x = 2
    in
     let x = 3
     in
      x

overwriteLambda x =
    (\x -> 3) ((\x -> 2) x)

inc_two x =
    (\x -> 
        (\x -> x + 1) x
    ) x + 1

-- First class functions
-- When we realize we're doing something multiple times,
-- we can abstract it away. In Haskell we can do that by
-- passing functions as arguments

ifEven someFunction x =
    if even x
    then someFunction x
    else x

-- Now we can use our double function from before
ifEvenDouble x = 
    ifEven double x

-- Write a lambda function for cubing x and pass it to ifEven
-- in ghci: ifEven (\x -> x*x*x) 6

