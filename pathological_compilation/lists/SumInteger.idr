
main : IO ()
main = print $ sum $ List.replicate 10000 (the Integer 11)
