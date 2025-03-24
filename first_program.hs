headerPart recipient = "Dear " ++ recipient ++ ",\n"
bodyPart title = "Thanks for buying " ++ title ++ "!\n"
endingPart author = "Thanks,\n" ++ author
createEmail :: [Char] -> [Char] -> [Char] -> [Char]
createEmail recipient title author = headerPart recipient ++
                                        bodyPart title ++
                                        endingPart author

main :: IO()
main = do
    print "Who is the email for?"
    recipient <- getLine
    print "What is the title?"
    title <- getLine
    print "Who is the author?"
    author <- getLine
    print (createEmail recipient title author)