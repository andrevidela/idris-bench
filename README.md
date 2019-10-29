# Idris-bench

Idris-bench is a tool to benchmark the idris2 compiler.

## Compiling

```
idris scripts/idris-bench.idr -o idris-bench
```

## Usage

```
idris-bench ((-p | --path PATH) | (-c | --commit COMMIT)) FOLDER
```

- `PATH` is the path to idris2
- `COMMIT` is the commit of idris2 to use
- `FOLDER` is the folder to use for benchmarks

the results are printed in the `results.txt` file

### Examples


The following will use the version of Idris 2 installed in your path and will 
run all the benchmarks in the `benchmark_idris2/fibonnacci` folder.

```
idris-bench -p $(which idris2) benchmark_idris2/fibonnacci
```


The following will use the branch `exprimental` and will run all benchmarks
in the `pathological_compilation` folder.

```
idris-bench -c experimental pathological_compilation
```

## Room for improvements

- [ ] run over n times 
- [ ] save which version of the compiler is used in the results
- [ ] better format results (csv?)
- [ ] select a file output for results instead of using `results.txt` all the time
- [ ] compare performance between two commits (?)
