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
