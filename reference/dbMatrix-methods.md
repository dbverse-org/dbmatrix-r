# Arith dbMatrix, e2

See [`methods::Arith`](https://rdrr.io/r/methods/S4groupGeneric.html)
for more details.

See [`methods::Arith`](https://rdrr.io/r/methods/S4groupGeneric.html)
for more details.

See [`methods::Arith`](https://rdrr.io/r/methods/S4groupGeneric.html)
for more details.

See [`methods::Ops`](https://rdrr.io/r/methods/S4groupGeneric.html) for
more details.

See [`methods::Ops`](https://rdrr.io/r/methods/S4groupGeneric.html) for
more details.

See [`methods::Ops`](https://rdrr.io/r/methods/S4groupGeneric.html) for
more details.

## Usage

``` r
# S4 method for class 'dbMatrix,ANY'
Arith(e1, e2)

# S4 method for class 'ANY,dbMatrix'
Arith(e1, e2)

# S4 method for class 'dbMatrix,dbMatrix'
Arith(e1, e2)

# S4 method for class 'dbMatrix,ANY'
Ops(e1, e2)

# S4 method for class 'ANY,dbMatrix'
Ops(e1, e2)

# S4 method for class 'dbMatrix,dbMatrix'
Ops(e1, e2)

# S4 method for class 'DBIConnection'
dbLoad(conn, name, class)

# S4 method for class 'dbMatrix'
writeMM(obj, file, ...)
```

## Arguments

- e1:

  First operand.

- e2:

  Second operand.

- conn:

  DBIConnection object

- name:

  valid name value (character)

- class:

  character, class of the dbMatrix object (e.g. "dbDenseMatrix" or
  "dbSparseMatrix")

- obj:

  dbMatrix object

- file:

  path to file

- ...:

  additional arguments

## Value

- Arithmetic and logical group methods return a
  [`dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md)
  object of the appropriate dense or sparse subclass, with the same
  dimensions as the input and transformed values stored in DuckDB.

- `dbLoad()` returns a
  [`dbDenseMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbDenseMatrix-class.md)
  or
  [`dbSparseMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbSparseMatrix-class.md)
  pointing to an existing DuckDB table.

- `writeMM()` writes a Matrix Market file to `file` and returns
  `invisible(TRUE)` on success.
