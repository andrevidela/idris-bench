
import Data.Vect

main : IO ()
main = putStrLn $ concat $ Vect.replicate 10000 "0123456789"
