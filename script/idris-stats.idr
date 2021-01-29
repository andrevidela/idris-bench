module Main

import System
import System.File
import Data.Vect
import Data.List1
import Data.Nat
import Data.Fin
import CSV
import Data.Strings

minMax : (Ord a) => List1 a -> (a, a)
minMax ls =
  let min = foldr1 (\e1, e2 => if e1 < e2 then e1 else e2) ls
      max = foldr1 (\e1, e2 => if e1 > e2 then e1 else e2) ls
   in (min, max)

minList : (Ord a) => List1 a -> a
minList = fst . minMax

maxList : Ord a => List1 a -> a
maxList = snd . minMax

-- slice all the dataset into n bucket and then return a vector that counts
-- how many occurences of the data appear for each bucket
-- graphData : Num n => (min, max : n) -> List1 n -> (buckets : Nat) -> Vect buckets Nat
-- graphData mn mx ds Z = []
-- graphData mn mx ds (S buckets) =
--   let diff = mx - mn
--       bucketed = map (findBucket mn mx buckets) ds
--    in foldr (\v, acc => insertBucket v acc) (replicate (S buckets) Z) bucketed
--   where
--
--     insertBucket : Nat -> Vect (S buckets) Nat -> Vect (S buckets) Nat
--     insertBucket n vs = let index = fromMaybe (the (Fin (S buckets)) FZ)
--                                               (natToFin n (S buckets)) in
--                             updateAt index S vs
--
--     findBucket : n -> n -> Nat -> n -> Nat
--     findBucket min max range v = let diff = max - min in fromInteger $ cast $
--         (cast range) * ((v - min) / diff)

||| Parse a row into its name and the data following it
parseRow : List String -> Maybe (String, List1 Integer)
parseRow (x :: y :: xs) = map (MkPair x) (traverse parsePositive (y ::: xs))
parseRow _ = Nothing

-- partial
-- printGraph : CSV -> IO ()
-- printGraph (c :: cs) =
--   let (Just parsed) = the (Maybe (List1 (String, List1 Integer))) (traverse parseRow (c ::: cs))
--       onlyResults = map snd parsed
--       minMaxLists = map minMax ?whut
--       superMin = minList (map fst minMaxLists)
--       superMax = maxList (map snd minMaxLists)
--    in traverse_ putStrLn (map (\x => show $ graphData superMin superMax x 30) onlyResults)

len : List1 a -> Nat
len (x ::: xs) = S (length xs)

||| Map a csv into another one containing the statistical analysis of each row
||| Keep the first element since it's the name that identifies the results
||| Map the rest as a list of double and compute the mean and variance
||| the resulting CSV has rows with 3 columns: name, mean and variance
compileResults : CSV -> Maybe (List CSV)
compileResults csv = traverse analyseData csv
  where

    ||| Mean of a list of Doubles
    computeMean : List1 Integer -> Integer
    computeMean ls = (sum ls) `div` (cast (len ls))

    ||| Variance given a mean and a list of doubles
    computeVar : Integer -> List1 Integer -> Integer
    computeVar mean ls = (sum (map (\x => x * x) (map ((-) mean) ls))) `div`
                         (cast (len ls))
    squareRoot : Integer -> Integer
    squareRoot = cast . sqrt . cast

    ||| Map a row to a data row containing the name, the mean and the variance
    analyseData : List String -> Maybe CSV
    analyseData row = do (name, pdata) <- parseRow row
                         let mean = computeMean pdata
                         let variance = computeVar mean pdata
                         let stddev = squareRoot variance
                         let stderr = stddev `div` squareRoot (cast $ len pdata)
                         let (min, max) = minMax pdata
                         pure [["filename", name]
                              ,["min", show min]
                              ,["max", show max]
                              ,["mean", show mean]
                              ,["stddev", show stddev]
                              ,["stderr", show stderr]
                              ]


partial
main : IO ()
main = do [_, filename] <- getArgs
            | _ => putStrLn "Expected 1 argument"
          Right fileContent <- readFile  filename
            | Left err => putStrLn (show err)
          let (Just csv) = parseCSV fileContent
            | Nothing => putStrLn ("could not parse " ++ filename ++ " as csv")
          let (Just results) = compileResults csv
            | Nothing => putStrLn "could not interpret csv content"
          putStrLn $ printCSV (join results)
          -- printGraph csv
          pure ()


