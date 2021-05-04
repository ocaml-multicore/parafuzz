# parafuzz
Multi-core OCaml concurrency bug detection tool

## Setup
1. Create parafuzz switch

```
opam switch create parafuzz --empty --repositories=parfuzz-multicore=git+https://github.com/SumitPadhiyar/multicore-opam.git,default
```

2. Install parafuzz

```
opam pin add -k path .
```

3. Install dune-2.7.1
```
opam install dune.2.7.1
```

## Install parafuzz-lib
```
opam pin add -k path otherlibs/parafuzz-lib/
```
