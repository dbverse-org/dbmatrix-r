#' Precompute a dbMatrix table in a database
#'
#' @param conn duckdb database connection
#' @param m number of rows of precomputed dbMatrix table
#' @param n number of columns of precomputed dbMatrix table
#' @param name name of the precomputed dbMatrix table to be created
#' @description
#' Precomputes a dbMatrix table in a specificied database connection.
#' This can speed up operations that involve breaking
#' sparsity of a \code{dbSparseMatrix},
#' such as in cases when performing + or - arithmetic operations.
#' Need only be run once.
#'
#' @details
#' The \code{num_rows} and \code{num_cols} parameters must exceed the
#' maximum row and column indices of the \code{dbMatrix} in order to be used for
#' densifying any \code{dbMatrix}. Should these params be less than the maximum
#' row and column indices the precomputed densification process will fail.
#'
#' In such cases, run this function again with a larger
#' \code{n_rows} and \code{num_cols}, or to manually remove the precomputed
#' table set \code{options(dbMatrix.precomp = NULL)} in the R console.
#'
#' @return NULL
#' @export
#'
#' @examples
#' con = DBI::dbConnect(duckdb::duckdb(), ":memory:")
#' precompute(con = con , m = 100, n = 100, name = "precomputed_table")
precompute <- function(conn, m, n, name){
  # input validation
  .check_con(conn = conn)
  if(name %in% DBI::dbListTables(conn)){
    options(dbMatrix.precomp = name)
    str <- glue::glue("Precomputed dbMatrix '{name}' with
                    {m} rows and {n} columns")
    cat(str, "\n")
    return()
  }
  .check_name(name = name)

  if(!(is.numeric(m) | is.integer(m)) | !(is.numeric(n) | is.integer(n))){
    stop("m and n must be integers or numerics")
  }

  # to prevent R-duckDB integer passing errors and permit >int32 indices
  n_rows = bit64::as.integer64(m)
  n_cols = bit64::as.integer64(n)

  sql_i <- glue::glue("SELECT i FROM generate_series(1, {n_rows}) AS t(i)")
  sequence_i <- dplyr::tbl(con, dplyr::sql(sql_i))

  sql_j <- glue::glue("SELECT j FROM generate_series(1, {n_cols}) AS t(j)")
  sequence_j <- dplyr::tbl(con, dplyr::sql(sql_j))

  key <- sequence_i |>
    dplyr::cross_join(sequence_j) |>
    dplyr::mutate(x = 0) |>
    dplyr::compute(temporary = FALSE, name = name)

  # set global variable for precomputed matrix
  options(dbMatrix.precomp = name)

  str <- glue::glue("Precomputed dbMatrix '{name}' with
                    {n_rows} rows and {n_cols} columns")
  cat(str, "\n")
}
