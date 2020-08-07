module Main

import System
import System.File
import Data.Vect
import Data.Nat
import Data.Fin
import CSV
import Data.Strings

minMax : List Double -> (Double, Double)
minMax ls =
  let min = foldr (\e1, e2 => if e1 < e2 then e1 else e2)
                  (the Double 4503599627370496) ls
      max = foldr (\e1, e2 => if e1 > e2 then e1 else e2)
                  (the Double 0) ls in
      (min, max)
minList : List Double -> Double
minList = fst . minMax

maxList : List Double -> Double
maxList = snd . minMax

-- slice all the dataset into n bucket and then return a vector that counts
-- how many occurences of the data appear for each bucket
graphData : (min, max : Double) -> List Double -> (buckets : Nat) -> Vect buckets Nat
graphData mn mx ds Z = []
graphData mn mx ds (S buckets) =
  let diff = mx - mn
      bucketed = map (findBucket mn mx buckets) ds
   in foldr (\v, acc => insertBucket v acc) (replicate (S buckets) Z) bucketed
  where

    insertBucket : Nat -> Vect (S buckets) Nat -> Vect (S buckets) Nat
    insertBucket n vs = let index = fromMaybe (the (Fin (S buckets)) FZ)
                                              (natToFin n (S buckets)) in
                            updateAt index S vs

    findBucket : Double -> Double -> Nat -> Double -> Nat
    findBucket min max range v = let diff = max - min in fromInteger $ cast $
        (cast range) * ((v - min) / diff)

||| Parse a row into its name and the data following it
parseRow : List String -> Maybe (String, List Double)
parseRow (x :: xs) = map (MkPair x) (traverse (\x => map (/ 100000) (parseDouble x)) xs)
parseRow _ = Nothing

partial
printGraph : CSV -> IO ()
printGraph csv = let (Just parsed) = traverse (\row => parseRow row) csv
                     onlyResults : List (List Double) = map (snd) parsed
                     minMaxLists = map minMax onlyResults
                     superMin = minList (map fst minMaxLists)
                     superMax = maxList (map snd minMaxLists)
                  in traverse_ putStrLn (map (\x => show $ graphData superMin superMax x 30) onlyResults)

||| Map a csv into another one containing the statistical analysis of each row
||| Keep the first element since it's the name that identifies the results
||| Map the rest as a list of double and compute the mean and variance
||| the resulting CSV has rows with 3 columns: name, mean and variance
compileResults : CSV -> Maybe CSV
compileResults csv = traverse analyseData csv
  where

    ||| Mean of a list of Doubles
    computeMean : List Double -> Double
    computeMean ls = (sum ls) / (cast (length ls))

    ||| Variance given a mean and a list of doubles
    computeVar : Double -> List Double -> Double
    computeVar mean ls = (sum (map (\x => x * x) (map ((-) mean) ls))) /
                         (cast (length ls))

    ||| Map a row to a data row containing the name, the mean and the variance
    analyseData : List String -> Maybe (List String)
    analyseData row = do (name, pdata) <- parseRow row
                         let mean = computeMean pdata
                         let variance = computeVar mean pdata
                         let min = foldr (\e1, e2 => if e1 < e2 then e1 else e2)
                                         (the Double 4503599627370496) pdata
                         let max = foldr (\e1, e2 => if e1 > e2 then e1 else e2)
                                         (the Double 0) pdata
                         pure [name, show min, show max, show mean, show variance]


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
          printGraph csv
          pure ()


