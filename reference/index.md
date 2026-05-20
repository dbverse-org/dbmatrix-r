# Package index

## S4 Classes

S4 class definitions for dbMatrix objects

- [`dbMatrix()`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md)
  : S4 virtual class for dbMatrix

- [`dbDenseMatrix-class`](https://dbverse-org.github.io/dbmatrix-r/reference/dbDenseMatrix-class.md)
  [`dbDenseMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbDenseMatrix-class.md)
  :

  S4 Class for `dbDenseMatrix`

- [`dbSparseMatrix-class`](https://dbverse-org.github.io/dbmatrix-r/reference/dbSparseMatrix-class.md)
  [`dbSparseMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbSparseMatrix-class.md)
  : S4 Class for dbSparseMatrix

## Coercion

Methods for coercing `dbMatrix` objects to other types

- [`coerce-dbMatrix-dgCMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/coerce-dbMatrix-dgCMatrix.md)
  [`coerce-dbDenseMatrix-dgCMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/coerce-dbMatrix-dgCMatrix.md)
  [`coerce-dbSparseMatrix-dgCMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/coerce-dbMatrix-dgCMatrix.md)
  : Coerce dbMatrix to dgCMatrix
- [`coerce-dbMatrix-matrix`](https://dbverse-org.github.io/dbmatrix-r/reference/coerce-dbMatrix-matrix.md)
  [`coerce-dbDenseMatrix-matrix`](https://dbverse-org.github.io/dbmatrix-r/reference/coerce-dbMatrix-matrix.md)
  [`coerce-dbSparseMatrix-matrix`](https://dbverse-org.github.io/dbmatrix-r/reference/coerce-dbMatrix-matrix.md)
  : Coerce dbMatrix to matrix
- [`coerce-matrix-dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/coerce-matrix-dbMatrix.md)
  [`coerce-dgCMatrix-dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/coerce-matrix-dbMatrix.md)
  : Coerce matrix to dbMatrix

## Constructor

Function for creating `dbMatrix` objects

- [`as.dbMatrix()`](https://dbverse-org.github.io/dbmatrix-r/reference/as.dbMatrix.md)
  :

  Convert [`Matrix::Matrix`](https://rdrr.io/pkg/Matrix/man/Matrix.html)
  to `dbMatrix`

- [`as.matrix(`*`<dbMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/as.matrix.dbMatrix.md)
  :

  Convert `dbMatrix` to in-memory matrix

- [`dbMatrix_from_tbl()`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix_from_tbl.md)
  : dbMatrix_from_tbl

- [`` `[`( ``*`<dbMatrix>`*`,`*`<dbIndex>`*`,`*`<missing>`*`,`*`<ANY>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/extract-dbMatrix.md)
  [`` `[`( ``*`<dbMatrix>`*`,`*`<missing>`*`,`*`<dbIndex>`*`,`*`<ANY>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/extract-dbMatrix.md)
  [`` `[`( ``*`<dbMatrix>`*`,`*`<dbIndex>`*`,`*`<dbIndex>`*`,`*`<ANY>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/extract-dbMatrix.md)
  [`` `[`( ``*`<dbMatrix>`*`,`*`<dbMatrix>`*`,`*`<missing>`*`,`*`<ANY>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/extract-dbMatrix.md)
  [`` `[<-`( ``*`<dbMatrix>`*`,`*`<dbMatrix>`*`,`*`<missing>`*`,`*`<ANY>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/extract-dbMatrix.md)
  [`` `[`( ``*`<dbMatrix>`*`,`*`<dbDenseMatrix>`*`,`*`<missing>`*`,`*`<ANY>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/extract-dbMatrix.md)
  [`` `[`( ``*`<dbMatrix>`*`,`*`<missing>`*`,`*`<dbDenseMatrix>`*`,`*`<ANY>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/extract-dbMatrix.md)
  [`` `[`( ``*`<dbMatrix>`*`,`*`<dbDenseMatrix>`*`,`*`<dbDenseMatrix>`*`,`*`<ANY>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/extract-dbMatrix.md)
  : Extract or replace values in database-backed matrices

- [`to_named_ijx_tbl()`](https://dbverse-org.github.io/dbmatrix-r/reference/to_named_ijx_tbl.md)
  : Convert dbMatrix to named ijx table

## Matrix Summary Operations

Methods to compute summary statistics for `dbMatrix` objects

- [`Summary(`*`<dbMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/Summary-dbMatrix-method.md)
  :

  Summary Methods for `dbMatrix` Objects

- [`mean(`*`<dbDenseMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/mean.md)
  [`mean(`*`<dbSparseMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/mean.md)
  :

  Arithmetic Mean for `dbMatrix` objects

- [`rowMeans(`*`<dbMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/row_col_means.md)
  [`colMeans(`*`<dbMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/row_col_means.md)
  :

  Row (column) means for `dbMatrix` objects

- [`colSds(`*`<dbDenseMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/row_col_sds.md)
  [`colSds(`*`<dbSparseMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/row_col_sds.md)
  [`rowSds(`*`<dbDenseMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/row_col_sds.md)
  [`rowSds(`*`<dbSparseMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/row_col_sds.md)
  :

  Row (column) standard deviations for `dbMatrix` objects

- [`rowSums(`*`<dbDenseMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/row_col_sums.md)
  [`rowSums(`*`<dbSparseMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/row_col_sums.md)
  [`colSums(`*`<dbDenseMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/row_col_sums.md)
  [`colSums(`*`<dbSparseMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/row_col_sums.md)
  :

  Row (column) sums for `dbMatrix` objects

- [`rowVars(`*`<dbDenseMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/row_col_vars.md)
  [`rowVars(`*`<dbSparseMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/row_col_vars.md)
  [`colVars(`*`<dbDenseMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/row_col_vars.md)
  [`colVars(`*`<dbSparseMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/row_col_vars.md)
  :

  Row (column) variances for `dbMatrix` objects

## Matrix Transformations

Methods for transforming `dbMatrix` objects

- [`Math(`*`<dbMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/Math-dbMatrix-method.md)
  :

  Math Operations for `dbMatrix` Objects

- [`is.na(`*`<dbMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/is.na-dbMatrix-method.md)
  : Element-wise is.na for dbMatrix

- [`` `%in%`( ``*`<dbDenseMatrix>`*`,`*`<ANY>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/percent-in.md)
  [`` `%in%`( ``*`<ANY>`*`,`*`<dbDenseMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/percent-in.md)
  [`` `%in%`( ``*`<dbSparseMatrix>`*`,`*`<ANY>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/percent-in.md)
  : Value Matching

- [`t(`*`<dbMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/t-dbMatrix.md)
  : Matrix Transpose

- [`compute(`*`<dbMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/compute.dbMatrix.md)
  : Force computation of a dbMatrix

- [`db_svd()`](https://dbverse-org.github.io/dbmatrix-r/reference/db_svd.md)
  : Perform Streaming SVD on a dbMatrix

## Matrix Properties

Methods to retrieve basic properties of `dbMatrix` objects

- [`dim(`*`<dbMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/dim-dbMatrix-method.md)
  : Dimensions of an Object

- [`head(`*`<dbMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/head_tail.md)
  [`tail(`*`<dbMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/head_tail.md)
  : Return the First or Last Parts of an Object

- [`length(`*`<dbMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/length.md)
  :

  Length of a `dbMatrix` Object

- [`rownames.dbMatrix()`](https://dbverse-org.github.io/dbmatrix-r/reference/matrix_props.md)
  [`` `rownames<-`( ``*`<dbMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/matrix_props.md)
  [`colnames.dbMatrix()`](https://dbverse-org.github.io/dbmatrix-r/reference/matrix_props.md)
  [`` `colnames<-`( ``*`<dbMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/matrix_props.md)
  [`dimnames(`*`<dbMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/matrix_props.md)
  [`` `dimnames<-`( ``*`<dbMatrix>`*`,`*`<list>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/matrix_props.md)
  : Retrieve and Set Row (Column) Dimension Names of dbMatrix Objects

- [`names(`*`<dbDenseMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/names-dbDenseMatrix-method.md)
  : The names of a dbMatrix Object

- [`nrow.dbMatrix()`](https://dbverse-org.github.io/dbmatrix-r/reference/nrow_ncol.md)
  [`ncol.dbMatrix()`](https://dbverse-org.github.io/dbmatrix-r/reference/nrow_ncol.md)
  : The Number of Rows/Columns of a dbMatrix Object

## dbData objects

Convenience methods for `dbData` objects

- [`Arith(`*`<dbMatrix>`*`,`*`<ANY>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix-methods.md)
  [`Arith(`*`<ANY>`*`,`*`<dbMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix-methods.md)
  [`Arith(`*`<dbMatrix>`*`,`*`<dbMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix-methods.md)
  [`Ops(`*`<dbMatrix>`*`,`*`<ANY>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix-methods.md)
  [`Ops(`*`<ANY>`*`,`*`<dbMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix-methods.md)
  [`Ops(`*`<dbMatrix>`*`,`*`<dbMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix-methods.md)
  [`dbLoad(`*`<DBIConnection>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix-methods.md)
  [`writeMM(`*`<dbMatrix>`*`)`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix-methods.md)
  : Arith dbMatrix, e2

## Package Options

Global options for the dbMatrix package

- [`dbMatrix_options`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix_options.md)
  [`dbMatrix-options`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix_options.md)
  : dbMatrix Package Global Options
