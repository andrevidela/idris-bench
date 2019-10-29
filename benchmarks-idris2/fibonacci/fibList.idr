
import Data.Vect

tailRecFib : Nat -> List Nat
tailRecFib Z = []
tailRecFib (S Z) = [1]
tailRecFib (S (S k)) = rec 1 1 [] k
  where
    rec : Nat -> Nat -> List Nat -> Nat -> List Nat
    rec prev curr ls Z = prev :: curr :: ls
    rec prev curr ls (S n) = rec (prev + curr) prev (curr :: ls) n

main : IO ()
main = printLn (tailRecFib 2000)
