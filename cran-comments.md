## Submission

This is a minor release with new features and bug fixes. One change is not
backward compatible, and it is described in NEWS: sparse operations that turn
0 into another value (such as `exp()` or `x == 0`) now account for the zeros
that are not stored, instead of returning incorrect results.

## Test environments

* local R 4.6.0 on x86_64-pc-linux-gnu (Ubuntu 24.04, WSL2)

## R CMD check results

`R CMD check --as-cran --no-manual` with `NOT_CRAN` unset

* 0 errors
* 0 warnings
* 1 note: "Compilation used the following non-portable flag(s):
  '-mno-omit-leaf-frame-pointer'". This flag comes from the local R
  installation's default compiler flags (Ubuntu's r-base), not from the
  package; `src/Makevars` sets no flags.

## Reverse dependencies

I checked the one reverse dependency on CRAN (dbProject 0.1.1) with
`R CMD check` against this version. It was OK.
