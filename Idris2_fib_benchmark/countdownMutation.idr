data IntData = MkInt Int

%mutating
countdown : (1 _ : IntData) -> IntData
countdown (MkInt 0) = (MkInt 0)
countdown (MkInt n) = countdown (MkInt (n-1))

main : IO ()
main = printLn (countdown (MkInt 50000000))
