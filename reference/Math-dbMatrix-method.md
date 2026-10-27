# Math Operations for [`dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md) Objects

Implements the `Math` `S4groupGeneric` functions for
[`dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md)
objects. This includes various mathematical operations such as
logarithms, exponentials, trigonometric functions, and other
transformations.

## Usage

``` r
# S4 method for class 'dbMatrix'
Math(x)
```

## Arguments

- x:

  A
  [`dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md)
  object.

## Value

A
[`dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md)
object with the mathematical operation applied to each element.

## Details

Supported functions:

- Rounding and sign: [`abs()`](https://rdrr.io/r/base/MathFun.html),
  [`sign()`](https://rdrr.io/r/base/sign.html),
  [`sqrt()`](https://rdrr.io/r/base/MathFun.html),
  [`ceiling()`](https://rdrr.io/r/base/Round.html),
  [`floor()`](https://rdrr.io/r/base/Round.html),
  [`trunc()`](https://rdrr.io/r/base/Round.html)

- Logarithms and exponentials:
  [`log()`](https://rdrr.io/r/base/Log.html),
  [`log10()`](https://rdrr.io/r/base/Log.html),
  [`log2()`](https://rdrr.io/r/base/Log.html),
  [`log1p()`](https://rdrr.io/r/base/Log.html),
  [`exp()`](https://rdrr.io/r/base/Log.html),
  [`expm1()`](https://rdrr.io/r/base/Log.html)

- Trigonometric: [`cos()`](https://rdrr.io/r/base/Trig.html),
  [`sin()`](https://rdrr.io/r/base/Trig.html),
  [`tan()`](https://rdrr.io/r/base/Trig.html),
  [`acos()`](https://rdrr.io/r/base/Trig.html),
  [`asin()`](https://rdrr.io/r/base/Trig.html),
  [`atan()`](https://rdrr.io/r/base/Trig.html),
  [`cosh()`](https://rdrr.io/r/base/Hyperbolic.html),
  [`sinh()`](https://rdrr.io/r/base/Hyperbolic.html),
  [`tanh()`](https://rdrr.io/r/base/Hyperbolic.html),
  [`acosh()`](https://rdrr.io/r/base/Hyperbolic.html),
  [`asinh()`](https://rdrr.io/r/base/Hyperbolic.html),
  [`atanh()`](https://rdrr.io/r/base/Hyperbolic.html)

- Special: [`gamma()`](https://rdrr.io/r/base/Special.html),
  [`lgamma()`](https://rdrr.io/r/base/Special.html)

Not supported: [`cumprod()`](https://rdrr.io/r/base/cumsum.html),
[`cospi()`](https://rdrr.io/r/base/Trig.html),
[`sinpi()`](https://rdrr.io/r/base/Trig.html),
[`tanpi()`](https://rdrr.io/r/base/Trig.html),
[`digamma()`](https://rdrr.io/r/base/Special.html) and
[`trigamma()`](https://rdrr.io/r/base/Special.html).
[`cumsum()`](https://rdrr.io/r/base/cumsum.html),
[`cummax()`](https://rdrr.io/r/base/cumsum.html) and
[`cummin()`](https://rdrr.io/r/base/cumsum.html) run but do not match
base R, which returns a vector.

Values outside a function's domain (for example `log(0)` on a dense
matrix or `acos(2)`) give a database error instead of `-Inf` or `NaN`.

**Sparse matrices**: a `dbSparseMatrix` stores only non-zero values.
Functions that turn 0 into something else, such as
[`exp()`](https://rdrr.io/r/base/Log.html) or
[`cos()`](https://rdrr.io/r/base/Trig.html), first fill in the zeros
(see `dbMatrix.allow_densify` in
[dbMatrix_options](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix_options.md)).
[`log()`](https://rdrr.io/r/base/Log.html),
[`log2()`](https://rdrr.io/r/base/Log.html) and
[`log10()`](https://rdrr.io/r/base/Log.html) are the exception: they
transform only the stored values and leave zeros at 0, so
`log(x + 1)`-style normalization can stay sparse.

**DuckDB Log Function Mappings**:

|            |                 |                          |
|------------|-----------------|--------------------------|
| R Function | DuckDB Function | Notes                    |
| `log(x)`   | `LN(x)`         | Natural logarithm        |
| `log10(x)` | `LOG10(x)`      | Base-10 logarithm        |
| `log2(x)`  | `LOG2(x)`       | Base-2 logarithm         |
| `log1p(x)` | `LN(x + 1)`     | log(1+x), computed as LN |

**Sparsity-Preserving Log**: For `dbSparseMatrix` with pending
operations, `log(x + 1)` operations preserve sparsity since
`log(0 + 1) = 0`. The multiplicative component is applied first, then
the log transformation is applied to sparse values only.

## Examples

``` r
mat <- matrix(1, nrow = 3, ncol = 3)
dbmat <- as.dbMatrix(mat)
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpGbMgtr/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
log(dbmat)
#> 3 x 3  dbMatrix of class "dbDenseMatrix"
#> [[ Colnames: 'col1', 'col2', 'col3' ]]
#>                                   
#> row1 0.0000000 0.0000000 0.0000000
#> row2 0.0000000 0.0000000 0.0000000
#> row3 0.0000000 0.0000000 0.0000000
sqrt(dbmat)
#> 3 x 3  dbMatrix of class "dbDenseMatrix"
#> [[ Colnames: 'col1', 'col2', 'col3' ]]
#>                                   
#> row1 1.0000000 1.0000000 1.0000000
#> row2 1.0000000 1.0000000 1.0000000
#> row3 1.0000000 1.0000000 1.0000000
sin(dbmat)
#> 3 x 3  dbMatrix of class "dbDenseMatrix"
#> [[ Colnames: 'col1', 'col2', 'col3' ]]
#>                                   
#> row1 0.8414710 0.8414710 0.8414710
#> row2 0.8414710 0.8414710 0.8414710
#> row3 0.8414710 0.8414710 0.8414710
```
