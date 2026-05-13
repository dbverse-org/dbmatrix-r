# Coercion Methods ####

#' @title Coerce dbMatrix to matrix
#' @name coerce-dbMatrix-matrix
#' @description
#' Coercion methods to convert `dbMatrix` objects to in-memory `matrix` objects.
#' Respects `dbMatrix.max_mem_convert` option to prevent OOM errors.
#' @return A base R [`matrix`] containing the collected matrix values with the
#'   same dimensions and dimnames as the source object.
setAs("dbMatrix", "matrix", function(from) {
  .check_mem_limit(from)
  as.matrix(from)
})

#' @rdname coerce-dbMatrix-matrix
#' @name coerce-dbDenseMatrix-matrix
setAs("dbDenseMatrix", "matrix", function(from) {
  .check_mem_limit(from)
  as.matrix(from)
})

#' @rdname coerce-dbMatrix-matrix
#' @name coerce-dbSparseMatrix-matrix
setAs("dbSparseMatrix", "matrix", function(from) {
  .check_mem_limit(from)
  as.matrix(from, sparse = FALSE)
})

#' @title Coerce dbMatrix to dgCMatrix
#' @name coerce-dbMatrix-dgCMatrix
#' @description
#' Coercion methods to convert `dbMatrix` objects to in-memory `dgCMatrix` objects.
#' Respects `dbMatrix.max_mem_convert` option to prevent OOM errors.
#' @return A [`Matrix::dgCMatrix-class`] object containing the collected matrix
#'   values. Dense inputs are converted to sparse Matrix format after collection.
setAs("dbMatrix", "dgCMatrix", function(from) {
  .check_mem_limit(from)
  as.matrix(from, sparse = TRUE)
})

#' @rdname coerce-dbMatrix-dgCMatrix
#' @name coerce-dbDenseMatrix-dgCMatrix
setAs("dbDenseMatrix", "dgCMatrix", function(from) {
  .check_mem_limit(from)
  mat <- as.matrix(from)
  as(mat, "dgCMatrix")
})

#' @rdname coerce-dbMatrix-dgCMatrix
#' @name coerce-dbSparseMatrix-dgCMatrix
setAs("dbSparseMatrix", "dgCMatrix", function(from) {
  .check_mem_limit(from)
  as.matrix(from, sparse = TRUE)
})

#' @title Coerce matrix to dbMatrix
#' @name coerce-matrix-dbMatrix
#' @description
#' Coercion methods to convert in-memory `matrix` objects to `dbMatrix` objects.
#' Creates a new in-memory DuckDB connection.
#' @return A database-backed matrix object. Dense inputs return a
#'   [`dbDenseMatrix`], while sparse [`Matrix::dgCMatrix-class`] inputs return a
#'   [`dbSparseMatrix`].
setAs("matrix", "dbMatrix", function(from) {
  con <- DBI::dbConnect(duckdb::duckdb())
  as.dbMatrix(from, con = con)
})

#' @rdname coerce-matrix-dbMatrix
#' @name coerce-dgCMatrix-dbMatrix
setAs("dgCMatrix", "dbMatrix", function(from) {
  con <- DBI::dbConnect(duckdb::duckdb())
  as.dbMatrix(from, con = con)
})
