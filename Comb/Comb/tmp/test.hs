
main = print $ foldl (\path x -> x:path) "" "abc"
