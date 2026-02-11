#' Perform Streaming SVD on a dbMatrix
#' @param dbm A dbSparseMatrix object
#' @param k Number of singular values to compute
#' @param center Logical, center rows (default TRUE)
#' @param scale Logical, scale rows (default FALSE)
#' @param center_rows Logical, center rows vs columns (default TRUE for standard PCA)
#' @param memory_limit Bytes for Fast Path. Set 0 to force BPCells path. Default 500 MB.
#' @param return_format "svd" (d, u, v) or "pca" (eigenvalues, loadings, coords)
#' @return List with SVD or PCA components
#' @export
db_svd <- function(dbm, k = 10, center = TRUE, scale = FALSE, center_rows = NULL,
                   memory_limit = getOption("dbMatrix.svd_memory", 500 * 1024^2),
                   return_format = c("svd", "pca")) {
  
  if (!requireNamespace("nanoarrow", quietly = TRUE))
    stop("Package 'nanoarrow' is required")
  
  if (is.null(center_rows)) center_rows <- TRUE
  
  # Column-wise centering/scaling not yet supported in C++ operator

  if (!center_rows && (center || scale))
    stop("center_rows=FALSE with center=TRUE or scale=TRUE is not yet supported")
  
  dbm <- .castNumeric(dbm) # Ensure numeric type
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
  
  # Compute row means/sds for normalization (center_rows=TRUE when center||scale)
  means <- if (center) as.numeric(rowMeans(dbm)) else numeric(0)
  sds <- numeric(0)
  if (scale) {
    sds <- as.numeric(rowSds(dbm))
    sds[is.na(sds) | sds == 0] <- 1
  }

  tbl <- dbm[]
  final_sql <- as.character(dbplyr::sql_render(tbl))

  # Estimate data size for path selection
  nnz_df <- dplyr::collect(tbl |> dplyr::count())
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
      k = as.integer(k)
    )
  } else {
    # BPCells Path: Stream to disk, disk-backed SVD
    if (!requireNamespace("BPCells", quietly = TRUE))
      stop("Package 'BPCells' required for large matrix SVD. Install with: remotes::install_github('bnprks/BPCells/r')")
    
    if (getOption("dbMatrix.verbose", TRUE))
      cli::cli_alert_info("Using BPCells File Path (streaming write, disk-backed SVD)")
    
    bp_dir <- tempfile(pattern = "bpcells_")
    dir.create(bp_dir)
    on.exit(unlink(bp_dir, recursive = TRUE), add = TRUE)
    
    sorted_sql <- paste0("SELECT * FROM (", final_sql, ") ORDER BY j")
    sorted_stream_factory <- function() {
      res <- DBI::dbSendQuery(con, sorted_sql, arrow = TRUE)
      reader <- duckdb::duckdb_fetch_arrow(res, chunk_size = 1000000)
      nanoarrow::as_nanoarrow_array_stream(reader)
    }
    
    write_result <- .write_arrow_to_bpcells_cpp(
      output_dir = bp_dir,
      n_rows = as.integer(n_rows),
      n_cols = as.integer(n_cols),
      col_scale = rep(1.0, n_cols),
      stream_factory = sorted_stream_factory
    )
    
    if (getOption("dbMatrix.verbose", TRUE))
      cli::cli_alert_success(paste0("Wrote ", format(write_result$nnz, big.mark = ","), " entries to BPCells format"))
    
    bp_mat <- BPCells::open_matrix_dir(bp_dir)
    
    # Implicit norm: A_norm = diag(row_scale) * A * diag(col_scale) + row_offset
    if (!all(row_scale == 1))
      bp_mat <- BPCells::multiply_rows(bp_mat, row_scale)
    
    if (!all(col_scale == 1))
      bp_mat <- BPCells::multiply_cols(bp_mat, col_scale)
    
    if (!all(row_offset == 0))
      bp_mat <- bp_mat + row_offset
    
    bp_result <- BPCells::svds(bp_mat, k = k)
    result <- list(d = bp_result$d, u = bp_result$u, v = bp_result$v)
  }
  
  # Format results
  u_mat <- result$u
  v_mat <- result$v
  d_vec <- result$d
  
  if (!is.null(rownames(dbm)) && nrow(u_mat) == nrow(dbm))
    rownames(u_mat) <- rownames(dbm)
  if (!is.null(colnames(dbm)) && nrow(v_mat) == ncol(dbm))
    rownames(v_mat) <- colnames(dbm)
  
  return_format <- match.arg(return_format)
  
  if (return_format == "pca") {
    coords <- sweep(v_mat, 2, d_vec, "*")
    colnames(coords) <- paste0("Dim.", seq_len(k))
    colnames(u_mat) <- paste0("Dim.", seq_len(k))
    list(eigenvalues = d_vec^2, loadings = u_mat, coords = coords)
  } else {
    list(d = d_vec, u = u_mat, v = v_mat)
  }
}
