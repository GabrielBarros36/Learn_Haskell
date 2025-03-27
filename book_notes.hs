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

-- Closures
-- This function is the most general: builds the request from scratch
getRequestURL host apiKey resource id = 
    host ++ 
    "/" ++
    resource ++
    "/" ++
    id ++
    "?token" ++
    apiKey

-- Now say we don't want to type the same host
-- in all the time. We can instead build functions
-- for frequently used URLs.
-- First, a builder function to enable that:
genHostRequestBuilder host = (\apiKey resource id ->
    getRequestURL host apiKey resource id)

-- Now we can build functions for specific hosts:
amazonRequestBuilder = genHostRequestBuilder "http://amazon.com"
-- To use on GHCI: 
-- amazonRequestBuilder "[apiKey]" "[resource]" "[id]"

-- Now we want to get more specific: we don't want to 
-- have to put in our API key all the time. We can
-- address that too!
-- Note that the arguments in the original genHostRequestBuilder
-- function are ordered by specificity.
genApiRequestBuilder hostBuilder apiKey = 
    (\resource id -> hostBuilder apiKey resource id)

amazonAPIKey1RequestBuilder = genApiRequestBuilder amazonRequestBuilder "amz-api-8rw9q"

-- We can get as specific as we want - let's make a
-- function for resources. 
genResourceBuilder apiBuilder resource = 
    \id -> apiBuilder resource id

-- Now using partial application: a builder function
-- that's specifically for http://amazon.com, the 
-- "amz-api-8rw9q" API key and the "books" resource.

amazonApiKey1BooksBuilder = 
    getRequestURL "http://amazon.com" "amz-api-8rw9q" "books"

-- Use flip and partial application to create a function
-- called subtract2 that removes 2 from whatever number
-- is passed in to it
subtract2 num = flip (-) 2

-- Q5.1 Redefine ifEvenInc, ifEvenDouble, and ifEvenSquare
-- by using ifEven and partial application
ifEvenInc num = ifEven (\x -> x+1)

-- or
-- inc x = x + 1
-- ifEvenInc = ifEven inc

-- Q5.2
binaryPartialApplication binFunc arg = 
    (\y -> binFunc arg y)

-- Lesson 6
-- Fun tidbit: lists are inherently recursive. A list is either empty
-- or it's an element followed by another list. 
-- Head: an element. Tail: a list.
-- A list is always some value consed with a list 
-- 1:[] = [1]
-- 1:2:3:[] = [1,2,3]
-- To access an index i of a list: list !! i
-- Or (!!) list i, like with any other infix operator
-- To get the length of a list: length [list-name]
-- To check if elem is in a list, use elem [list-name] - Returns true or false

-- Any binary function can be treated as an infix operator by wrapping
-- it in back-quotes (`). 
-- E.g. elem 13 [0,1,2] is the same as 13 `elem` [0,1,2]

-- take returns the first n elements of a list
-- E.g. take 2 [1,2,3,4,5,6] = [1,2]
-- drop removes the first n elements of a list

-- zip combines two lists into tuple pairs, stopping at the shorter list
-- E.g. zip ['a' .. 'c'] [1..] = [('a',1),('b',2),('c',3)]

-- cycle creates an infinite list out of some list
-- E.g. cycle[1] = [1,1,1,...]
-- E.g. ones n = take n (cycle [1]) - then ones 3 = [1,1,1]

-- Q6.1: implement your version of repeat
myRepeat list = cycle list
-- Q6.2: write a function that returns the subsequence between a given
-- starting and ending positions in a list 
mySubseq start end list = take (end-start) (drop start list)
-- Q6.3 Write a function inFirstHalf that returns True if an element
-- is in the first half of a list and False otherwise
inFirstHalf thing list = elem thing (take (len `div` 2) list)
    where len = length list

-- Lesson 7

