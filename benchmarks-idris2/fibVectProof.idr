
import Data.Nat
import Data.Vect

tailRecFib' : (n : Nat) -> Vect n Nat
tailRecFib' Z = []
tailRecFib' (S Z) = [1]
tailRecFib' (S (S l)) = rec [1,1] l
  where
    rec : Vect (2 + k) Nat -> (n : Nat) -> Vect ((2 + k) + n) Nat
    rec vs Z {k} = rewrite plusZeroRightNeutral k in vs
    rec vs@(p :: c :: xs) (S l) {k} = rewrite sym $ plusSuccRightSucc k l in
                                              rec (p + c :: vs) l

main : IO ()
main = printLn (tailRecFib' 2000)
