# Advent of Code 2024

This repository contains the source code for solution for [Advent of Code 2024](https://adventofcode.com/2024).

Solutions for each day can be run using

```console
cabal run day<N> -- <inputs>
```

If compilation with performance optimizations is required, use a different project file:

```console
cabal run day<N> --project-file=cabal.project.optimized -- <inputs>
```

If compilation with profiling is required, use yet another project file, and turn on profiling in the runtime system using `+RTS -p`:

```console
cabal run day<N> --project-file=cabal.project.profiling -- <inputs> +RTS -p
```

Note that BLAS and LAPACK are required for certain days involving linear algebra. On Ubuntu:

```console
sudo apt-get install libblas-dev liblapack-dev
```