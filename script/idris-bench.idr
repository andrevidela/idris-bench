
import System

import Data.Vect
import Data.List
import Data.Strings
import Data.Maybe
import Data.Either
import System
import System.Clock
import System.File
import System.Directory

import CSV

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

convertMaybe : Maybe a -> Either BenchError a
convertMaybe = maybeToEither (MkError "unexepect null value")

||| given a program that runs in IO, measure the time it takes to run
measureRunTime : Lazy (IO Int) -> IO (Int, Clock Duration)
measureRunTime program = do begin <- clockTime Process
                            ret <- program
                            end <- clockTime Process
                            pure $ (ret, end `timeDifference` begin)

----------------------------------------------------------------------------------------------------
-- Program measurement -----------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

||| Run a program and measure the time it takes
||| returns Nothing if the exit code was different than 0
measureRunTime' : String -> Lazy (IO Int) -> BenchmarkM (Clock Duration)
measureRunTime' progName program = do
  (ret, time) <- measureRunTime program
  if ret /= 0
     then returnErr $ "program \"" ++ progName ++ "\" did not exit with 0"
     else returnVal time

runTime : String -> BenchmarkM (Clock Duration)
runTime programName = do
  putStrLn $ "measuring runtime for program " ++ programName
  measureRunTime' programName (system programName)

runPrograms : List String -> BenchmarkM (List (String, Clock Duration))
runPrograms [] = returnVal []
runPrograms (x :: xs) = do Right result <- runTime x
                             | Left _ => returnErr ("failed to run program " ++ x)
                           Right rest <- runPrograms xs
                             | Left (MkError err) => returnErr err
                           returnVal $ (x, result) :: rest

-- Tree for Filesystem

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
  Right dir <- openDir path
    | Left _ => pure False
  closeDir dir
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
    notDot str = assert_total $ prim__strHead str /= '.'

||| List all files in a directory
ls : String -> IO (Either FileError (List String))
ls name = do Right d <- openDir name | Left err => pure (Left err)
             result <- lsAcc d Nothing
             closeDir d
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
BenchResults = Tree (String, List (Clock Duration))

compileFile : (idris, file, name : String) -> BenchmarkM ()
compileFile idris file name = do
  putStrLn $ "compile with name: " ++ name
  putStrLn $ "running idris: "
  putStrLn $ idris ++ " " ++ file ++ " -o " ++ name
  0 <- system $ idris ++ " " ++ file ++ " -o " ++ name
    | _ => returnErr (
          "could not compile file " ++ file ++ " into binary " ++ name)
  putStrLn "done compiling"
  returnVal  ()

||| Given an idris path, a file to compile and a destination for it, compile and
||| benchmark the file
compileAndBenchmarkBinary : (idris, file, name : String) -> Int -> BenchmarkM BenchResults
compileAndBenchmarkBinary idris file name repetitions =
  do putStrLn "compile and benchmark"
     Right () <- compileFile idris file name
       | Left (MkError err) => returnErr err
     let program = "build/exec/" ++ name
     Right duration <- doubleTraverse (const $ runTime program) [0..repetitions]
       | Left (MkError err) => returnErr err
     returnVal $ TreeLeaf (file, duration)


compileIdris : (commit : String) -> BenchmarkM ()
compileIdris commit = do 0 <- system "make clean"
                           | _ => returnErr "Could not clean"
                         0 <- system $ "git checkout " ++ commit
                           | _ => returnErr "Could not checkout"
                         0 <- system $ "make all"
                           | _ => returnErr "Could not compile Idris"
                         returnVal ()

||| Get the path to Idris
getPathToBinary : (folder, commit : String) -> BenchmarkM ()
getPathToBinary folder path =
  do Just curr <- currentDir
       | Nothing => returnErr "Could not get current dir"
     changeDir folder
     compileIdris path
     changeDir curr
     returnVal ()

writeBenchFile : BenchResults -> BenchmarkM ()
writeBenchFile results = do Right file <-  writeFile "results.txt" (show results)
                              | Left err => pure (Left (fileToBenchError err))
                            putStrLn "successfully wrote file"
                            returnVal ()

removeFileExension : String -> String
removeFileExension path = pack $ takeWhile (/= '.') $ unpack path


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

parameters (idrisPath : String, reps: Int)
  benchmarkTree : Vect n String -> FileSystem -> BenchmarkM BenchResults
  benchmarkTree path t@(TreeNode dirName files) = do
    putStrLn "benchmarking directory:"
    print t
--    Right () <- createDir' (pathFromVect ((map (++ "_bin") (dirName :: path))))
--      | Left _ => returnErr "fileError"
    putStrLn $ "about to traverse directory " ++ dirName
    Right rec <- doubleTraverse (benchmarkTree (dirName :: path)) files
      | Left (MkError err) => returnErr err
    returnVal $ TreeNode (dirName, []) rec
  benchmarkTree path (TreeLeaf filename)      = do
    putStrLn $ "benchmarking file " ++ filename
    putStrLn $ "its full path is " ++ show path
    let totalPath = (filename :: path)
    putStrLn $ "pathVect is " ++ pathFromVect totalPath
    compileAndBenchmarkBinary idrisPath (pathFromVect totalPath) (removeFileExension filename ++ "_bin") reps

cleanupfiles : FileSystem -> FileSystem
cleanupfiles = filterLeaves isIdr
  where
    isIdr : String -> Bool
    isIdr x = let extension = dropWhile (/= '.') $ unpack x
               in unpack ".idr" == extension

benchmarkDirectory : (idris, root : String) -> Int -> BenchmarkM BenchResults
benchmarkDirectory idris root reps = do
  Right (TreeNode name files) <- lsRec [root]
    | Left _ => returnErr "Filesystem error"
    | Right whut => returnErr ("unexpected value: " ++ show whut)
  putStrLn $ "Here is the tree : " ++ show files
  Right result <- benchmarkTree idris reps [] ((TreeNode name files))
    | Left (MkError err) => returnErr err
  print result
  returnVal result

data TestOutput = StdOut | FilePath String

-- description of which Idris2 binary to use for testing
data IdrisBin = IdrisPath String
              | IdrisCompile
                  String -- Folder where Idris2 is
                  String -- commit to use

-- Options that the program needs to operates
record IdrisOptions where
  constructor MkOptions
  idrisTesting : IdrisBin -- the path to the Idris2 binary that we use to compile Idris2
  -- Either the path to Idris2 that we use for testing OR
  -- A pair with the path and the commit to use compile a version of idris that will
  -- run the tests
  testPath : String -- The directory where the tests are located
  testResults : TestOutput -- The output for this test suite
  testCount : Int -- The number of times a test will be run

getIdris2Path : IdrisBin -> BenchmarkM String
getIdris2Path (IdrisPath path) = returnVal path
getIdris2Path (IdrisCompile folder commit) = do
  getPathToBinary folder commit
  returnVal $ folder ++ "/build/exec/idris2"

toNano : Clock type -> Integer
toNano (MkClock seconds nanoseconds) =
  let scale = 1000000000
   in scale * seconds + nanoseconds

resultsToCSV : List String -> BenchResults -> List (List String)
resultsToCSV path (TreeNode (p, _) xs) =
  let rec = traverse (resultsToCSV (p :: path)) xs in concat rec
resultsToCSV path (TreeLeaf (_, times)) =
  [ concat path :: map (show . toNano) times ]

-- Given a set of options, run the benchmarks that the options describe
execBenchmarks : IdrisOptions -> BenchmarkM String
execBenchmarks opts = do
  Right idris2Bin <- getIdris2Path opts.idrisTesting
    | Left err => pure $ Left err
  Right results <- benchmarkDirectory idris2Bin (opts.testPath) (opts.testCount)
    | Left err => pure $ Left err
  returnVal (printCSV $ resultsToCSV [] results)

parseOptions : List String -> BenchmarkM IdrisOptions
parseOptions ["-p", idrisPath, "-t", testPath, "-o", fileoutput, "-c", count] =
  returnVal $ MkOptions (IdrisPath idrisPath) (testPath) (FilePath fileoutput) (cast count)
parseOptions ["-p", idrisPath, "-t", testPath, "--stdout", "-c", count] =
  returnVal $ MkOptions (IdrisPath idrisPath) (testPath) (StdOut) (cast count)
parseOptions ["--compile", compilerFolder, commit, "-t", testPath, "-o", output, "-c", count] =
  returnVal $ MkOptions (IdrisCompile compilerFolder commit) testPath (FilePath output) (cast count)
parseOptions ["--compile", compilerFolder, commit, "-t", testPath, "--stdout",  "-c", count] =
  returnVal $ MkOptions (IdrisCompile compilerFolder commit) testPath (StdOut) (cast count)
parseOptions _ = returnErr "Wrong options"

outputResults : String -> TestOutput -> IO ()
outputResults output StdOut = putStrLn output
outputResults output (FilePath path) = map (const ()) $ writeFile path output

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
main = do (_ :: xs) <- getArgs
            | _ => putStrLn "expected arguments"
          Right opts <- parseOptions xs
            | Left (MkError err) => putStrLn err
          Right csv <- execBenchmarks opts
            | Left (MkError err) => putStrLn err
          outputResults csv (opts.testResults)
          exitSuccess


