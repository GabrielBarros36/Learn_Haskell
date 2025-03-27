{-
Planning
We want to turn a simple .txt file into a well-formatted .md file based
on a few rules, which are applied on a line-by-line basis:
"+++[text]" -> ### **[text]**
"++[text]"  -> #### [text]
"+[text]"   -> - [text]

So some functions we want:
* turn file into string (readFile)
* separate string into list of strings, separating by \n character
* Detect +, ++, +++ in each line
* Transform each of the three variants above according to rules
-}

import System.IO (readFile)
import Data.List (isInfixOf)

-- Turn a file into a list:
-- readFile "filepath"

-- "+[text]" -> - [text]
onePlusTransform line = 
    '-' : ' ' : (drop 1 line)

twoPlusTransform line =
    "#### " ++ (drop 2 line)

threePlusTransform line =
    "### **" ++ drop 3 (reverse ("**" ++ (reverse line)))

-- numPlus should be zero when called
getTransformFunction line =
    if  "+++" `isInfixOf` line
        then threePlusTransform
    else if "++" `isInfixOf` line
        then twoPlusTransform
    else
        onePlusTransform

transformLine line = 
    func line
    where func = getTransformFunction line

{-
main :: IO()
main = do
    contents <- readFile "projects/test.txt"
-}