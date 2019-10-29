
import Data.Vect

main : IO ()
main = print $ sum $ Vect.replicate 10000 (the Nat 11)
