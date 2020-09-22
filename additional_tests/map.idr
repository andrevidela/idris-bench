module Main

mutating
mapList : (f : (1 _ : a) -> b) -> (1 _ : List a) -> List b
mapList f [] = []
mapList f (x :: xs) = f x :: mapList f xs

main : IO ()
main = printLn (length (mapList S [0 .. 9999]
