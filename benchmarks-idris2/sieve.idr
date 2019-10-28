module Main

concat : List a -> Lazy (Stream a) -> Stream a
concat [] ys = ys
concat (x :: xs) ys = x :: (Main.concat xs ys)

generate : (Num a, Ord a) => (start, step, max : a) -> List a
generate start step max = if start <= max
                             then start :: generate (start + step) step max
                             else []

mutual
  sieve : Nat -> Stream Int -> Int -> Stream Int
  sieve k (p::ps) x = Main.concat (start) (sieve (k + 1) ps (p * p))
    where
      fs : List Int
      fs = take k (tail primes)
      start : List Int
      start = [n | n <- (generate (x + 2) 2 (p * p - 2)), (all (\i => (n `mod` i) /= 0) fs)]

  primes : Stream Int
  primes = 2 :: 3 :: sieve 0 (tail primes) 3


main : IO()
main = printLn $ take 10 primes
