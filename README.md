# parafuzz-lib
Library to be used with parafuzz multicore ocaml distribution 

## Setup
1. Create parafuzz switch

```
opam switch create parafuzz --empty --repositories=parfuzz-multicore=git+https://github.com/SumitPadhiyar/multicore-opam.git,default
```

2. Install parafuzz

```
opam pin add -k path [parafuzz-path]
```

3. Install dune-2.7.1
```
opam install dune.2.7.1
```

## Install parafuzz-lib
```
opam pin add -k path [parafuzz-lib path]
```

## Run test
```
make run_test
```
