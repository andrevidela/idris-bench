
main : IO ()
main = putStrLn $ concat $ List.replicate 10000 "0123456789"
