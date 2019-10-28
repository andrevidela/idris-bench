
import System

%include C "unistd.h"
%include C "stdio.h"
%include C "time.h"


defaultIdrisLocation : String
defaultIdrisLocation = "Idris2/"
defaultIdrisBinary : String
defaultIdrisBinary = "Idris2/idris2"

record BenchError where
  constructor MkError
  error : String

Show BenchError where
  show (MkError msg) = msg

fileToBenchError : FileError -> BenchError
fileToBenchError x = MkError (show x)

data Tree a = TreeNode a (List (Tree a))
            | TreeLeaf a

padding : Nat -> String
padding k = concat $ replicate k "  "

printTree : Show a => (depth : Nat) -> Tree a -> String
printTree depth (TreeNode x xs) = let rec = map (printTree (S depth)) xs in
                                      padding depth ++ "|-" ++ show x ++ "\n"
                                        ++ unlines rec
printTree depth (TreeLeaf x) = padding depth ++ "|-" ++ show x

Show a => Show (Tree a) where
  show tree = printTree Z tree

FileSystem : Type
FileSystem = Tree String

||| Benchmark results with a nice tree structure
BenchResults : Type
BenchResults = Tree (String, Clock)

IdrisM : Type -> Type
IdrisM a = IO (Either BenchError a)

returnErr : String -> IdrisM a
returnErr = pure . Left . MkError

returnVal : a -> IdrisM a
returnVal = pure . Right

Show Clock where
  show (MkClock s n) = show s ++ "s " ++ show n ++ "ns"

clockDiff : Clock -> Clock -> Clock
clockDiff (MkClock secl nanl) (MkClock secr nanr) = MkClock (secl - secr) (nanl - nanr)

||| given a program that runs in IO, measure the time it takes to run
measureRunTime : Lazy (IO ()) -> IO Clock
measureRunTime program = do begin <- clockTime
                            _ <- program
                            end <- clockTime
                            pure $ end `clockDiff` begin

||| Run a program and measure the time it takes
||| returns Nothing if the exit code was different than 0
measureRunTime' : Lazy (IO Int) -> IdrisM Clock
measureRunTime' program = do
  begin <- clockTime
  if !(program) /= 0
     then returnErr $ "program did not exit with 0"
     else do end <- clockTime
             returnVal (end `clockDiff` begin)

runTime : String -> IdrisM Clock
runTime programName = measureRunTime' (system programName)

runPrograms : List String -> IdrisM (List (String, Clock))
runPrograms [] = returnVal []
runPrograms (x :: xs) = do Right result <- runTime x
                             | Left _ => returnErr ("failed to run program " ++ x)
                           Right rest <- runPrograms xs
                             | Left (MkError err) => returnErr err
                           returnVal $ (x, result) :: rest

compileIdris : String -> IdrisM ()
compileIdris commit = do 0 <- system "idris --clean idris2.ipkg"
                           | _ => returnErr "could not clean"
                         0 <- system $ "git checkout " ++ commit
                           | _ => returnErr "could not checkout"
                         0 <- system "idris --install idris2.ipkg"
                           | _ => returnErr "could not install"
                         returnVal ()

compileFile : (idris, file, dest : String) -> IO ()
compileFile idris file dest = do 0 <- system $ idris ++ " " ++ file ++ " -o " ++ dest
                                   | _ => exitFailure
                                 pure ()

||| Given an idris path, a file to compile and a destination for it, compile and
||| benchmark the file
compileAndBenchmarkBinary : (idris, file, dest : String) -> IdrisM BenchResults
compileAndBenchmarkBinary idris file dest =
  do compileFile idris file dest
     Right duration <- runTime dest
       | Left (MkError err) => returnErr err
     returnVal $ TreeLeaf (file, duration)

isDirectory : String -> IO Bool
isDirectory path = do
  Right dir <- dirOpen path
    | Left _ => pure False
  dirClose dir
  pure True

||| Maybe returns an error or nothing if everything goes well
createBinaryFolder : (path : String) -> IdrisM ()
createBinaryFolder path =
  let binPath = path ++ "_bin" in
      (either (Left . fileToBenchError) Right) <$> createDir binPath

||| Get the path to Idris
getPathToBinary : String -> IdrisM String
getPathToBinary path =
  do changeDir defaultIdrisLocation
     compileIdris path
     changeDir ".."
     returnVal defaultIdrisBinary

writeBenchFile : BenchResults -> IdrisM ()
writeBenchFile results = do Right file <-  writeFile "results.txt" (show results)
                              | Left err => pure (Left (fileToBenchError err))
                            putStrLn "successfully wrote file"
                            returnVal ()


||| Given an Idris path, a file/directory to benchmark and an output file execute and write the file
benchmarkFile : (idris, file, output : String) -> IdrisM BenchResults
benchmarkFile idris file output = do
  let fileBin = file ++ "_bin.so"
  compileAndBenchmarkBinary idris file fileBin


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
          Right idrisPath <- the (IdrisM String) $ case mode of
                "-p"       => returnVal idris
                "--path"   => returnVal idris
                "-c"       => returnErr "Unsupported"
                "--commit" => returnErr "Unsupported"
                v          => returnErr ("unknown flag" ++ v)
            | Left err => do putStrLn (show err) ; exitFailure
          Right results <- if !(isDirectory path)
                               then returnErr "Unsupported"
                               else benchmarkFile idrisPath path "index.txt"
            | Left err => do putStrLn (show err) ; exitFailure
          writeBenchFile results
          exitSuccess


