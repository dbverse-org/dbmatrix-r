#' Perform Streaming SVD on a dbMatrix
#' @param dbm A dbSparseMatrix object
#' @param k Number of singular values to compute
#' @param center Logical, center rows (default TRUE)
#' @param scale Logical, scale rows (default FALSE)
#' @param center_rows Logical, center rows vs columns (default TRUE for standard PCA)
#' @param memory_limit Bytes for Fast Path. Default 8 GB.
#' @param return_format "svd" (d, u, v) or "pca" (eigenvalues, loadings, coords)
#' @param tol Eigensolver convergence tolerance.
#' @param maxit Maximum eigensolver iterations.
#' @param ncv Number of Lanczos basis vectors. Defaults to
#'   `max(ceiling(1.5 * k), 40)`.
#' @param score_threads Number of threads used to calculate the cell scores.
#' @param operator_threads Number of threads used for each Gram-matrix product.
#' @return List with SVD or PCA components
#' @export
db_svd <- function(dbm, k = 10, center = TRUE, scale = FALSE, center_rows = NULL,
                   memory_limit = getOption("dbMatrix.svd_memory", 8 * 1024^3),
                   return_format = c("svd", "pca"),
                   tol = getOption("dbMatrix.svd_tol", 1e-5),
                   maxit = getOption("dbMatrix.svd_maxit", 1000L),
                   ncv = getOption("dbMatrix.svd_ncv", NULL),
                   score_threads = getOption("dbMatrix.svd_score_threads", 1L),
                   operator_threads = getOption("dbMatrix.svd_operator_threads", 1L)) {
  phase_started <- proc.time()[["elapsed"]]
  phase_time <- function(start) proc.time()[["elapsed"]] - start
  
  if (!requireNamespace("nanoarrow", quietly = TRUE))
    stop("Package 'nanoarrow' is required")
  
  if (is.null(center_rows)) center_rows <- TRUE
  
  # Column-wise centering/scaling not yet supported in C++ operator

  if (!center_rows && (center || scale))
    stop("center_rows=FALSE with center=TRUE or scale=TRUE is not yet supported")
  
  cast_started <- proc.time()[["elapsed"]]
  dbm <- .castNumeric(dbm) # Ensure numeric type
  cast_sec <- phase_time(cast_started)
  con <- get_con(dbm)
  dims <- dim(dbm)
  n_rows <- dims[1]
  n_cols <- dims[2]
  
  # Validate dimensions

  if (n_rows == 0 || n_cols == 0)
    stop("Matrix must have at least one row and one column")
  
  # Validate k (Spectra requires k <= n_rows - 1 for the Gram operator)
  k <- as.integer(k)
  max_k <- min(n_rows - 1L, n_cols)
  if (k < 1) stop("k must be at least 1")
  if (max_k < 1) stop("Matrix too small for SVD (need at least 2 rows)")
  if (k > max_k) {
    warning(sprintf("k=%d exceeds max allowed (%d); reducing", k, max_k))
    k <- max_k
  }

  tol <- as.numeric(tol)
  maxit <- as.integer(maxit)
  if (length(tol) != 1L || !is.finite(tol) || tol <= 0)
    stop("tol must be one positive finite number")
  if (length(maxit) != 1L || is.na(maxit) || maxit < 1L)
    stop("maxit must be one positive integer")
  if (is.null(ncv)) ncv <- min(max(as.integer(ceiling(1.5 * k)), 40L), n_rows)
  ncv <- as.integer(ncv)
  if (length(ncv) != 1L || is.na(ncv) || ncv <= k || ncv > n_rows)
    stop("ncv must be greater than k and no greater than the number of rows")
  score_threads <- as.integer(score_threads)
  if (length(score_threads) != 1L || is.na(score_threads) || score_threads < 1L)
    stop("score_threads must be one positive integer")
  operator_threads <- as.integer(operator_threads)
  if (length(operator_threads) != 1L || is.na(operator_threads) || operator_threads < 1L)
    stop("operator_threads must be one positive integer")

  return_format <- match.arg(return_format)
  
  # Compute row means/sds for normalization (center_rows=TRUE when center||scale)
  row_stats_started <- proc.time()[["elapsed"]]
  means <- if (center) as.numeric(rowMeans(dbm)) else numeric(0)
  sds <- numeric(0)
  if (scale) {
    sds <- as.numeric(rowSds(dbm))
    sds[is.na(sds) | sds == 0] <- 1
  }
  row_stats_sec <- phase_time(row_stats_started)

  query_started <- proc.time()[["elapsed"]]
  tbl <- dbm[]
  final_sql <- as.character(dbplyr::sql_render(tbl))
  query_planning_sec <- phase_time(query_started)

  # Estimate data size for path selection
  count_started <- proc.time()[["elapsed"]]
  nnz_df <- dplyr::collect(tbl |> dplyr::count())
  nnz_count_sec <- phase_time(count_started)
  nnz <- as.numeric(nnz_df$n[1])
  if (is.null(nnz) || is.na(nnz) || nnz == 0) nnz <- n_rows * n_cols

  data_size <- nnz * 16
  use_cache <- memory_limit > 0 && data_size <= memory_limit

  if (getOption("dbMatrix.verbose", TRUE)) {
    if (!use_cache && memory_limit > 0) {
      cli::cli_alert_info("Data size ({format(structure(data_size, class='object_size'), units='auto')}) exceeds limit - using streaming iteration")
    }
  }

  stream_factory <- function() {
    res <- DBI::dbSendQuery(con, final_sql, arrow = TRUE)
    reader <- duckdb::duckdb_fetch_arrow(res, chunk_size = 1000000)
    nanoarrow::as_nanoarrow_array_stream(reader)
  }

  if (!is(dbm, "dbSparseMatrix"))
    stop("db_svd currently only supports dbSparseMatrix objects")

  # Determine row_offset, row_scale, col_scale for implicit normalization
  # A_norm = diag(row_scale) * A * diag(col_scale) + row_offset
  row_offset <- if (center) -means else rep(0, n_rows)
  row_scale <- rep(1, n_rows)
  col_scale <- rep(1, n_cols)

  if (scale) {
    inv_sds <- 1 / sds
    row_scale <- as.numeric(inv_sds)
    # Adjust offset: centering after scaling means offset = -mean/sd
    if (center) row_offset <- -means * inv_sds
  }
  
  if (use_cache) {
    # Fast Path: Arrow -> Eigen CSC
    if (getOption("dbMatrix.verbose", TRUE)) {
      cli::cli_alert_info("Data size ({format(structure(data_size, class='object_size'), units='auto')}) - Arrow/Eigen (CSC)")
    }
    
    result <- .compute_op_svd_arrow_cpp(
      stream_factory = stream_factory,
      n_rows = as.integer(n_rows),
      n_cols = as.integer(n_cols),
      row_offset = as.numeric(row_offset),
      row_scale = as.numeric(row_scale),
      col_scale = as.numeric(col_scale),
      k = as.integer(k),
      tol = tol,
      maxit = maxit,
      ncv = ncv,
      pca_scores = identical(return_format, "pca"),
      score_threads = score_threads,
      operator_threads = operator_threads,
      expected_nnz = nnz
    )
  } else {
    stop("Estimated SVD input exceeds memory_limit; increase memory_limit to use the in-memory Arrow/Eigen path.")
  }
  
  # Set dimnames while each matrix is owned only by the C++ result list.
  # Extracting first and then modifying the alias can duplicate a very large
  # cells-by-components matrix under R's copy-on-modify rules.
  formatting_started <- proc.time()[["elapsed"]]
  component_names <- if (return_format == "pca") paste0("PC", seq_len(k)) else NULL
  if (!is.null(rownames(dbm)) && nrow(result$u) == nrow(dbm)) {
    dimnames(result$u) <- list(rownames(dbm), component_names)
  } else if (!is.null(component_names)) {
    colnames(result$u) <- component_names
  }
  if (!is.null(colnames(dbm)) && nrow(result$v) == ncol(dbm)) {
    dimnames(result$v) <- list(colnames(dbm), component_names)
  } else if (!is.null(component_names)) {
    colnames(result$v) <- component_names
  }
  formatting_sec <- phase_time(formatting_started)

  diagnostics <- list(
    niter = result$niter,
    nops = result$nops,
    matrix_build_sec = result$matrix_build_sec,
    arrow_stream_sec = result$arrow_stream_sec,
    csc_build_sec = result$csc_build_sec,
    eigensolver_sec = result$eigensolver_sec,
    score_projection_sec = result$score_projection_sec,
    score_threads = result$score_threads,
    operator_threads = result$operator_threads,
    cast_sec = cast_sec,
    row_stats_sec = row_stats_sec,
    query_planning_sec = query_planning_sec,
    nnz_count_sec = nnz_count_sec,
    formatting_sec = formatting_sec,
    total_sec = phase_time(phase_started),
    tol = tol,
    maxit = maxit,
    ncv = ncv
  )
  if (isTRUE(getOption("dbMatrix.svd_verbose_timing", FALSE))) {
    message(
      "db_svd timing (seconds): cast=", sprintf("%.3f", cast_sec),
      ", row_stats=", sprintf("%.3f", row_stats_sec),
      ", query_planning=", sprintf("%.3f", query_planning_sec),
      ", nnz_count=", sprintf("%.3f", nnz_count_sec),
      ", stream_and_matrix_build=", sprintf("%.3f", result$matrix_build_sec),
      " [stream=", sprintf("%.3f", result$arrow_stream_sec),
      ", csc=", sprintf("%.3f", result$csc_build_sec), "]",
      ", eigensolver=", sprintf("%.3f", result$eigensolver_sec),
      ", score_projection=", sprintf("%.3f", result$score_projection_sec),
      ", formatting=", sprintf("%.3f", formatting_sec),
      ", total=", sprintf("%.3f", diagnostics$total_sec)
    )
  }

  if (return_format == "pca") {
    out <- list(
      eigenvalues = result$d^2,
      loadings = result$u,
      coords = result$v
    )
  } else {
    out <- list(d = result$d, u = result$u, v = result$v)
  }
  attr(out, "diagnostics") <- diagnostics
  out
}
