import Data.List

absolute num =
    if num > 0 
    then num
    else num * (-1)

inc num = 
    num + 1

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
ifEvenDouble :: Integral t => t -> t
ifEvenDouble x = 
    ifEven double x

-- Write a lambda function for cubing x and pass it to ifEven
-- ghci: ifEven (\x -> x*x*x) 6

-- Cool example for first-class functions: the sortby function.
-- It takes 2 arguments: the collection to be sorted 
-- AND a function to compare two objects in that collection.
-- Let's define a function to compare names

compareLastNames name1 name2 =
    if lastName1 > lastName2
        then GT
    else if lastName1 < lastName2
        then LT
    else EQ
    where 
        lastName1 = snd name1 
        lastName2 = snd name2 -- fst and snd access 1st and 
                              -- 2nd elements of a tuple
names = [ ("Bob", "Ross"),
          ("Bob", "Widlar"),
          ("Gabriel", "Romanini")]
-- ghci: sortBy compareLastNames names

-- Q4.1: Rewrite compareLastNames by using compare
compareLastNames2 name1 name2 =
    case compare lastName1 lastName2 of
        LT -> LT
        EQ -> EQ
        GT -> GT
    where
        lastName1 = snd name1
        lastName2 = snd name2


-- Q4.2: 
dcOffice name =
    nameText ++ " - " ++ "Washington, DC"
    where 
        nameText = fst name ++ " " ++ snd name ++ ", Esq."

getLocationFunction location = 
    case location of
        "dc" -> dcOffice
        _ -> (\name -> (fst name) ++ " " ++ (snd name))

addressLetter name location =
    func name
    where func = getLocationFunction location
-- Now if we need to add different locations with different
-- address formatting rules we can easily add cases to 
-- getLocationFunction