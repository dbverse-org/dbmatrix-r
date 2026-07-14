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

This method provides implementations for the following Math functions:

*Arithmetic and rounding*:

- [`abs()`](https://rdrr.io/r/base/MathFun.html),
  [`sign()`](https://rdrr.io/r/base/sign.html),
  [`sqrt()`](https://rdrr.io/r/base/MathFun.html),
  [`ceiling()`](https://rdrr.io/r/base/Round.html),
  [`floor()`](https://rdrr.io/r/base/Round.html),
  [`trunc()`](https://rdrr.io/r/base/Round.html)

*Cumulative operations*:

- [`cummax()`](https://rdrr.io/r/base/cumsum.html),
  [`cummin()`](https://rdrr.io/r/base/cumsum.html),
  [`cumprod()`](https://rdrr.io/r/base/cumsum.html),
  [`cumsum()`](https://rdrr.io/r/base/cumsum.html)

- **Note: [`cumprod()`](https://rdrr.io/r/base/cumsum.html) is not
  supported**

*Logarithmic*:

- [`log()`](https://rdrr.io/r/base/Log.html),
  [`log10()`](https://rdrr.io/r/base/Log.html),
  [`log2()`](https://rdrr.io/r/base/Log.html),
  [`log1p()`](https://rdrr.io/r/base/Log.html)

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

*Trigonometric*:

- [`cos()`](https://rdrr.io/r/base/Trig.html),
  [`sin()`](https://rdrr.io/r/base/Trig.html),
  [`tan()`](https://rdrr.io/r/base/Trig.html),
  [`acos()`](https://rdrr.io/r/base/Trig.html),
  [`asin()`](https://rdrr.io/r/base/Trig.html),
  [`atan()`](https://rdrr.io/r/base/Trig.html)

- [`cosh()`](https://rdrr.io/r/base/Hyperbolic.html),
  [`sinh()`](https://rdrr.io/r/base/Hyperbolic.html),
  [`tanh()`](https://rdrr.io/r/base/Hyperbolic.html),
  [`acosh()`](https://rdrr.io/r/base/Hyperbolic.html),
  [`asinh()`](https://rdrr.io/r/base/Hyperbolic.html),
  [`atanh()`](https://rdrr.io/r/base/Hyperbolic.html)

- [`cospi()`](https://rdrr.io/r/base/Trig.html),
  [`sinpi()`](https://rdrr.io/r/base/Trig.html),
  [`tanpi()`](https://rdrr.io/r/base/Trig.html)

- **Note: [`acosh()`](https://rdrr.io/r/base/Hyperbolic.html)
  [`asinh()`](https://rdrr.io/r/base/Hyperbolic.html)
  [`atanh()`](https://rdrr.io/r/base/Hyperbolic.html) are not
  supported**

*Exponential*:

- [`exp()`](https://rdrr.io/r/base/Log.html),
  [`expm1()`](https://rdrr.io/r/base/Log.html)

- **Note: [`expm1()`](https://rdrr.io/r/base/Log.html) is not
  supported**

*Special functions*:

- [`gamma()`](https://rdrr.io/r/base/Special.html),
  [`lgamma()`](https://rdrr.io/r/base/Special.html),
  [`digamma()`](https://rdrr.io/r/base/Special.html),
  [`trigamma()`](https://rdrr.io/r/base/Special.html)

- **Note: [`digamma()`](https://rdrr.io/r/base/Special.html)
  [`trigamma()`](https://rdrr.io/r/base/Special.html) are not
  supported**

The function applies the specified mathematical operation to each
element of the
[`dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md)
object.

## Examples

``` r
mat <- matrix(1, nrow = 3, ncol = 3)
dbmat <- as.dbMatrix(mat)
#> duckdb is keeping downloaded extensions in a temporary directory:
#> ℹ /tmp/RtmpWGOFG0/duckdb/extensions
#> This is removed when the R session ends, so extensions are re-downloaded each session.
#> ℹ To keep them, point `options(duckdb.extension_directory =)` or the `DUCKDB_EXTENSION_DIRECTORY` environment variable at a permanent path.
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
