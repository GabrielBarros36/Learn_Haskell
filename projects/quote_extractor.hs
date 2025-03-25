import Data.List

quotes = [
    ("AAPL", 187.52), 
    ("MSFT", 423.65),
    ("AMZN", 185.24),
    ("NVDA", 924.73),
    ("GOOGL", 173.84),
    ("META", 485.12),
    ("TSLA", 172.63),
    ("JPM", 198.47),
    ("V", 276.35),
    ("JNJ", 152.93)
    ]

compareQuotePrices stock1 stock2 =
    case compare price1 price2 of
        LT -> LT
        EQ -> EQ
        GT -> GT
    where 
        price1 = snd stock1
        price2 = snd stock2

compareStockNames stock1 stock2 =
    case compare name1 name2 of
        LT -> LT
        EQ -> EQ
        GT -> GT
    where 
        name2 = fst stock2
        name1 = fst stock1

getComparisonFunction criterium =
    case criterium of
        "price" -> compareQuotePrices
        "name" -> compareStockNames
        _ -> compare

main :: IO()
main = do
    print "How do you want to sort?"
    criterium <- getLine
    print (sortBy (getComparisonFunction criterium) quotes)