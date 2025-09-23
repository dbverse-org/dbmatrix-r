#' dbMatrix Package Global Options
#' @description The following global options can be modified to control the
#' behavior of the [`dbMatrix`] package.
#' @details Use `options()` to set the below options.
#' @section Options:
#' * [`dbMatrix.dbdm_auto_compute`]: logical. If `TRUE`, automatically
#' computes intermediate dense COO tables that are generated from
#' [`dbSparseMatrix`] to [`dbDenseMatrix`] conversion in the internal
#' function [`.to_db_dense`].
#' If `FALSE` (default), this step is skipped and the dense COO table is lazy.
#' Setting this to `TRUE` is recommended to avoid large intermediate COO tables
#' that can trigger downstream bottlenecks or memory errors due to disk spilling
#' when using DuckDB as a backend.
#' * [`dbMatrix.digits`]: integer. Number of digits to round to in the show
#' function of dbMatrix objects. Default is 7.
#'
#' @name dbMatrix-options
#' @aliases dbMatrix.dbdm_auto_compute
#' @keywords internal
NULL
