module Main

%mutating
mapList :  (1 _ : List a) -> List b
mapList [] = []
mapList (x :: xs) = x + 1 :: mapList f xs

main : IO ()
main = printLn (length (mapList [0 .. 9999]))
