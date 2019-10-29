
import Data.Vect

main : IO ()
main = print $ sum $ Vect.replicate 1000 (the Int 11)
