module Main

%mutating
mapList :  (List Int) -> List Int
mapList [] = []
mapList (x :: xs) = x + 1 :: mapList xs

main : IO ()
main = printLn (length (mapList [0 .. 9999]))
