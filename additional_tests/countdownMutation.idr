data IntData = MkInt Int | MkNat Nat

Show IntData where
  show (MkInt int) = show int
  show (MkNat nat) = show nat

%mutating
countdown : (IntData) -> IntData
countdown (MkInt 0) = MkInt 0
countdown (MkInt n) = countdown (MkInt (n-1))
countdown (MkNat Z) = MkNat Z
countdown (MkNat (S n)) = countdown (MkNat n)

main : IO ()
main = printLn (countdown (MkInt 50000000))
