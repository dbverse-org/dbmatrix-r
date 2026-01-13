#' dbMatrix Package Global Options
#' @description The following global options can be modified to control the
#' behavior of the [`dbMatrix`] package.
#' @details Use `options()` to set the below options.
#' @section Options:
#' * `dbMatrix.digits`: integer. Number of digits to round to in the show
#' function of dbMatrix objects. Default is 7.
#' * `dbMatrix.max_mem_convert`: numeric. Maximum size (in bytes) allowed for
#' implicit conversion of `dbMatrix` to in-memory matrix. Default is 8 * 1024^3 (8GB).
#' * `dbMatrix.chunk_size`: integer. Number of columns to process per chunk during
#' densification (`.to_db_dense`). Smaller chunks reduce memory usage but may increase
#' execution time. Default is `NULL` (automatically calculated based on `dbMatrix.max_mem_convert`).
#' * `dbMatrix.max_chunks`: integer. Maximum number of chunks allowed during
#' densification. This prevents query parser errors caused by excessive `UNION ALL`
#' branches. Default is 10000.
#' * `dbMatrix.verbose`: logical. If `TRUE` (default), prints informative messages
#' during implicit coercion.
#' * `dbMatrix.precomp_db`: character. Path to an external DuckDB file containing
#' precomputed table(s). If set, `dbMatrix` will automatically attach this
#' database (read-only) and look for a suitable precomputed table to speed up densification.
#' * `dbMatrix.allow_densify`: logical. If `FALSE` (default), automatic sparse-to-dense
#' conversion is disabled. This prevents unexpected disk spilling and memory issues
#' when operations would require densification (e.g., division by zero, scalar addition
#' to sparse matrix). Set to `TRUE` to enable on-disk dense conversion. **Warning**:
#' Dense conversion can cause massive disk usage for large matrices.
#'
#' @name dbMatrix_options
#' @aliases dbMatrix-options
NULL
