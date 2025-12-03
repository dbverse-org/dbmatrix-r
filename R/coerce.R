# Coercion Methods ####

#' @title Coerce dbMatrix to matrix
#' @name coerce-dbMatrix-matrix
#' @description
#' Coercion methods to convert `dbMatrix` objects to in-memory `matrix` objects.
#' Respects `dbMatrix.max_mem_convert` option to prevent OOM errors.
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
