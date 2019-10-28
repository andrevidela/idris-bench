
import Data.Vect


tailRecFib : Nat -> Nat
tailRecFib Z = 1
tailRecFib (S Z) = 1
tailRecFib (S (S k)) = rec 1 1 k
  where
    rec : Nat -> Nat -> Nat -> Nat
    rec prev curr Z = prev + curr
    rec prev curr (S j) = rec curr (prev + curr) j

main : IO ()
main = printLn $ tailRecFib 2000
