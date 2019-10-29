
import System

import Data.Vect

defaultIdrisLocation : String
defaultIdrisLocation = "Idris2/"
defaultIdrisBinary : String
defaultIdrisBinary = "Idris2/idris2"

----------------------------------------------------------------------------------------------------
-- Program analysis --------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

record BenchError where
  constructor MkError
  error : String

Show BenchError where
  show (MkError msg) = msg

fileToBenchError : FileError -> BenchError
fileToBenchError x = MkError (show x)

BenchmarkM : Type -> Type
BenchmarkM a = IO (Either BenchError a)

returnErr : String -> BenchmarkM a
returnErr = pure . Left . MkError

returnVal : a -> BenchmarkM a
returnVal = pure . Right

Show Clock where
  show (MkClock s n) = show s ++ "s " ++ show n ++ "ns"

sumClock : (lhs, rhs : Clock) -> Clock
sumClock (MkClock ls ln) (MkClock rs rn) = MkClock (ls + rs) (ln + rn)

clockDiff : Clock -> Clock -> Clock
clockDiff (MkClock secl nanl) (MkClock secr nanr) = MkClock (secl - secr) (nanl - nanr)

||| given a program that runs in IO, measure the time it takes to run
measureRunTime : Lazy (IO ()) -> IO Clock
measureRunTime program = do begin <- clockTime
                            _ <- program
                            end <- clockTime
                            pure $ end `clockDiff` begin

----------------------------------------------------------------------------------------------------
-- Program measurement -----------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

||| Run a program and measure the time it takes
||| returns Nothing if the exit code was different than 0
measureRunTime' : String -> Lazy (IO Int) -> BenchmarkM Clock
measureRunTime' progName program = do
  begin <- clockTime
  if !(program) /= 0
     then returnErr $ "program \"" ++ progName ++ "\" did not exit with 0"
     else do end <- clockTime
             returnVal (end `clockDiff` begin)

runTime : String -> BenchmarkM Clock
runTime programName = do
  putStrLn $ "measuring runtime for program " ++ programName
  measureRunTime' programName (system programName)

runPrograms : List String -> BenchmarkM (List (String, Clock))
runPrograms [] = returnVal []
runPrograms (x :: xs) = do Right result <- runTime x
                             | Left _ => returnErr ("failed to run program " ++ x)
                           Right rest <- runPrograms xs
                             | Left (MkError err) => returnErr err
                           returnVal $ (x, result) :: rest


data Tree a = TreeNode a (List (Tree a))
            | TreeLeaf a

foldTree : (b -> a -> b) -> b -> Tree a -> b
foldTree f acc (TreeNode x xs) = let folded = foldl (foldTree f) acc xs in f folded x
foldTree f acc (TreeLeaf x) = f acc x

total
filterLeaves : (a -> Bool) -> Tree a -> Tree a
filterLeaves f (TreeNode x xs) = TreeNode x (filter (precompose f) xs)
  where
    precompose : (a -> b) -> Tree a -> b
    precompose f (TreeNode x ys) = f x
    precompose f (TreeLeaf x) = f x
filterLeaves f (TreeLeaf x) = TreeLeaf x

padding : Nat -> String
padding k = concat $ List.replicate k "  "

printTree : Show a => (depth : Nat) -> Tree a -> String
printTree depth (TreeNode x xs) = let rec = map (printTree (S depth)) xs in
                                      padding depth ++ "|-" ++ show x ++ "\n"
                                        ++ unlines rec
printTree depth (TreeLeaf x) = padding depth ++ "|-" ++ show x

Show a => Show (Tree a) where
  show tree = printTree Z tree

----------------------------------------------------------------------------------------------------
-- FileSystem --------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

FileSystem : Type
FileSystem = Tree String

isDirectory : String -> IO Bool
isDirectory path = do
  Right dir <- dirOpen path
    | Left _ => pure False
  dirClose dir
  pure True

||| Ls with accumulator, uses `dirEntry` to get to the next entry
lsAcc : Directory -> Maybe (List String) -> IO (Either FileError (List String))
lsAcc d files = do Right file <- dirEntry d
                     | Left err => pure (maybe
                                           (Left err) -- if we did not start collecting files, error
                                           (Right . filter notDot) -- else return the files we found
                                           files)
                   lsAcc d (Just (file :: (maybe [] id files)))
  where
    ||| Don't pay attention to files starting with a dot
    notDot : String -> Bool
    notDot "" = True
    notDot str = strHead str /= '.'


||| List all files in a directory
ls : String -> IO (Either FileError (List String))
ls name = do Right d <- dirOpen name | Left err => pure (Left err)
             result <- lsAcc d Nothing
             dirClose d
             pure result

||| like partition : (a -> Bool) -> List a -> (List a, List a) but in a monad
partitionM : Monad m => (a -> m Bool) -> List a -> m (List a, List a)
partitionM p [] = pure ([], [])
partitionM p (x :: xs) = do (yes, no) <- partitionM p xs
                            case !(p x) of
                                 True => pure (x :: yes, no)
                                 False => pure (yes, x :: no)

||| Traverse twice, once with function, once to flip applicatives around
doubleTraverse : Applicative n => Applicative m => Traversable l =>
                 (a -> m (n d)) -> l a -> m (n (l d))
doubleTraverse f x = traverse id <$> traverse f x

||| Given a vector of path components a
pathFromVect : Vect n String -> String
pathFromVect xs = concat $ intersperse "/" $ reverse $ toList xs

||| Construct a tree of all files in a directory structure
lsRec : Vect (S n) String -> IO (Either FileError FileSystem)
lsRec path@(directory :: ds) = do
    Right files <- ls (pathFromVect path) | Left err => pure (Left err)
    (dirs, files) <- partitionM (isDirectory . pathFromVect . (:: path)) files
    Right subTree <- doubleTraverse (\d => lsRec (d :: path)) dirs
      | Left err => pure (Left err)
    pure $ Right (TreeNode directory (subTree ++ map TreeLeaf files))

----------------------------------------------------------------------------------------------------
-- Benchmarks --------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

||| Benchmark results with a nice tree structure
BenchResults : Type
BenchResults = Tree (String, Clock)


compileIdris : String -> BenchmarkM ()
compileIdris commit = do 0 <- system "idris --clean idris2.ipkg"
                           | _ => returnErr "could not clean"
                         0 <- system $ "git checkout " ++ commit
                           | _ => returnErr "could not checkout"
                         0 <- system "idris --install idris2.ipkg"
                           | _ => returnErr "could not install"
                         returnVal ()

compileFile : (idris, file, dest : String) -> BenchmarkM ()
compileFile idris file dest = do
  putStrLn $ "compiling file " ++ file ++ " at destination " ++ dest
  0 <- system $ idris ++ " " ++ file ++ " -o " ++ dest
    | _ => returnErr (
          "could not compile file " ++ file ++ " at destination " ++ dest)
  putStrLn "done compiling"
  returnVal  ()

||| Given an idris path, a file to compile and a destination for it, compile and
||| benchmark the file
compileAndBenchmarkBinary : (idris, file, dest : String) -> BenchmarkM BenchResults
compileAndBenchmarkBinary idris file dest =
  do Right () <- compileFile idris file dest
       | Left (MkError err) => returnErr err
     Right duration <- runTime (dest ++ ".so")
       | Left (MkError err) => returnErr err
     returnVal $ TreeLeaf (file, duration)


||| Get the path to Idris
getPathToBinary : String -> BenchmarkM String
getPathToBinary path =
  do changeDir defaultIdrisLocation
     compileIdris path
     changeDir ".."
     returnVal defaultIdrisBinary

writeBenchFile : BenchResults -> BenchmarkM ()
writeBenchFile results = do Right file <-  writeFile "results.txt" (show results)
                              | Left err => pure (Left (fileToBenchError err))
                            putStrLn "successfully wrote file"
                            returnVal ()

removeFileExension : String -> String
removeFileExension path = pack $ takeWhile (/= '.') $ unpack path

||| Given an Idris path and a file/directory to benchmark execute and write the file
benchmarkFile : (idris, file : String) -> BenchmarkM BenchResults
benchmarkFile idris file = do
  let fileBin = removeFileExension file ++ "_bin"
  putStrLn $ "benchmarking file " ++ fileBin
  compileAndBenchmarkBinary idris file fileBin

updateLast : (a -> a) -> Vect n a -> Vect n a
updateLast f [] = []
updateLast f (x :: []) = [f x]
updateLast f (x :: (y :: xs)) = x :: updateLast f (y :: xs)

createDir' : String -> IO (Either FileError ())
createDir' name = do Right _ <- createDir name
                       | Left err => do putStrLn ("failed to create dir " ++ name)
                                        pure (Left err)
                     putStrLn ("Created directory " ++ name)
                     pure (Right ())

benchmarkTree : String -> Vect n String -> FileSystem -> BenchmarkM BenchResults
benchmarkTree idris path t@(TreeNode dirName files) = do
  putStrLn "benchmarking directory:"
  print t
  Right () <- createDir' (pathFromVect ((map (++ "_bin") (dirName :: path))))
    | Left _ => returnErr "fileError"
  putStrLn $ "about to traverse directory " ++ dirName
  Right rec <- doubleTraverse (benchmarkTree idris (dirName :: path)) files
    | Left (MkError err) => returnErr err
  let totalTime = foldl (foldTree (\c, (_, c') => c `sumClock` c')) (MkClock 0 0) rec
  returnVal $ TreeNode (dirName, totalTime) rec
benchmarkTree idris path (TreeLeaf filename)      = do
  putStrLn $ "benchmarking file " ++ filename
  putStrLn $ "its full path is " ++ show path
  let totalPath = (filename :: path)
  let targetPath =  map (++ "_bin") (removeFileExension filename :: path)
  putStrLn $ "totalPath " ++ pathFromVect totalPath
  putStrLn $ "targetPath " ++ pathFromVect targetPath
  compileAndBenchmarkBinary idris (pathFromVect totalPath) (pathFromVect targetPath)

cleanupfiles : FileSystem -> FileSystem
cleanupfiles = filterLeaves isIdr
  where
    isIdr : String -> Bool
    isIdr x = let extension = dropWhile (/= '.') $ unpack x
               in unpack ".idr" == extension

benchmarkDirectory : (idris, root : String) -> BenchmarkM BenchResults
benchmarkDirectory idris root = do
  Right (TreeNode name files) <- lsRec [root]
    | Left _ => returnErr "Filesystem error"
    | whut => returnErr ("unexpected value: " ++ show whut)
  Right result <- benchmarkTree idris [] ((TreeNode name files))
    | Left (MkError err) => returnErr err
  fRemove (name ++ "_bin")
  print result
  returnVal result

||| Usage:
||| idris-bench ((-p | --path PATH) | (-c | --commit COMMIT)) FILE
|||
||| Idris bench will then either use the path to idris or compile idris
||| at the given commit.
||| then it will use that version to compile and run the target file.
||| If the file is isolated idris-bench will put the compile program beside
||| the source file.
||| If the file passed is a directory, a `_bin` directory will be crated beside
||| it and all the binaries will be put in that `_bin` directory and then
||| executed and benchmarked
||| the result will be put in a `result.txt` file
||| Examples :
|||
|||     idris-bench -p ~/.idris2/bin/idris2 benchmarks/fast/fibonacci/fibList.idr
|||     This will compile and benchmark the `fibList.idr` file using `idris2`
|||     The binary will be in benchmarks/fast/fibonacci/fibList
|||
|||     idris-bench --path ~/.idris2/bin/idris2 benchmarks/fast
|||     This will compile and benchmark all the `.idr` files in the `fast` folder
|||     The binaries will be in benchmarks/fast_bin/
|||
|||     idris-bench --commit 1234abcd benchmarks/
|||     This will compile idris2 at the 1234abcd commit and then compile and run
|||     all the benchmarks. The binaries will be in benchmarks_bin
main : IO ()
main = do [_, mode, idris, path] <- getArgs
            | _ => putStrLn "Wrong args expected 3"
          Right idrisPath <- the (BenchmarkM String) $ case mode of
                "-p"       => returnVal idris
                "--path"   => returnVal idris
                "-c"       => returnErr "Unsupported build from commit"
                "--commit" => returnErr "Unsupported build from commit"
                v          => returnErr ("unknown flag" ++ v)
            | Left err => do putStrLn (show err) ; exitFailure
          Right results <- if !(isDirectory path)
                               then benchmarkDirectory idrisPath path
                               else benchmarkFile idrisPath path
            | Left err => do putStrLn (show err) ; exitFailure
          writeBenchFile results
          exitSuccess


