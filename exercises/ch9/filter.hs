multiplesOf3 = filter (\x -> (rem x 3) == 0)

howManyMultiplesOf3 = length . multiplesOf3

noArticles :: String -> Bool
noArticles "the" = False
noArticles "a"   = False
noArticles "an"  = False
noArticles _     = True

filterArticles = filter (noArticles) . words
