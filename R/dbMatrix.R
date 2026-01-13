# initialize dbMatrix####

# Method for initializing dbMatrix. Concerns only the processing that is related
# to elements internal to the object.

# For pre-object construction data operations/massaging, see the constructor
# function dbMatrix()
#' @keywords internal
#' @noRd
setMethod(
  f = 'initialize',
  signature(.Object = 'dbMatrix'),
  function(.Object, dim_names, dims, ...) {
    # call dbMatrix initialize
    .Object <- methods::callNextMethod(.Object, ...)

    # matrix specific data input #
    # -------------------------- #
    if (!missing(dim_names)) {
      .Object@dim_names <- dim_names
    }
    if (!missing(dims)) {
      .Object@dims <- dims
    }

    # default values if no input provided #
    # ----------------------------------- #
    if (is.null(.Object@value)) {
      .Object@dims <- c(0L, 0L)
      .Object@dim_names <- list(NULL, NULL)
    }
    # tbl_name = dbplyr::remote_name(.Object[])
    # .Object@name = ifelse(is.null(tbl_name), NA_character_, tbl_name)

    # check and return #
    # ---------------- #
    validObject(.Object)
    return(.Object)
  }
)

# show ####

## dbDenseMatrix ####
setMethod("show", signature("dbDenseMatrix"), function(object) {
  row_names <- rownames(object)
  col_names <- colnames(object)
  dims <- dim(object)
  dim_row <- dims[1]
  dim_col <- dims[2]

  # print class and dims #
  # -------------------- #

  if (identical(dims, c(0L, 0L))) {
    cat('0 x 0 matrix of class "dbDenseMatrix"\n')
    return()
  } else if (ncol(object) == 1) {
    cat(dim_row, 'x 1 dbMatrix of class "dbDenseMatrix"\n')
  } else {
    cat(dim_row, "x", dim_col, ' dbMatrix of class "dbDenseMatrix"\n')
  }

  # preview print #
  # ------------- #

  # print colnames
  colname_show_n <- dim_col - 6L
  if (ncol(object) == 1) {
    colname_show_n <- 0L
  } else if (colname_show_n < 0L) {
    message("[[ Colnames: ", vector_to_string(col_names), " ]]")
  } else if (colname_show_n >= 1L) {
    message(
      "[[ Colnames ",
      vector_to_string(head(col_names, 3L)),
      " ... suppressing ",
      colname_show_n,
      " ...",
      vector_to_string(tail(col_names, 3L)),
      " ]]"
    )
  }

  # get matrix i and j to print
  suppress_rows <- FALSE # flag for whether rows are being suppressed

  # Determine column indices to show (first 10)
  if (dim_col > 10L) {
    filter_j <- seq_len(10L)
    p_coln <- col_names[filter_j]
  } else {
    filter_j <- seq_len(dim_col)
    p_coln <- col_names
  }

  # Determine row indices to show (head 3, tail 3 if large)
  if (dim_row > 6L) {
    filter_i <- c(seq_len(3L), seq(from = dim_row - 2L, to = dim_row))
    p_rown <- row_names[filter_i]
    suppress_rows <- TRUE
  } else {
    filter_i <- seq_len(dim_row)
    p_rown <- row_names
  }

  # prepare subset to print
  if (suppress_rows) {
    # Head query
    df_head <- object@value |>
      dplyr::filter(j <= 10L, i <= 3L) |>
      dplyr::arrange(i, j) |>
      dplyr::collect()

    # Tail query
    df_tail <- object@value |>
      dplyr::filter(j <= 10L, i >= (dim_row - 2L)) |>
      dplyr::arrange(i, j) |>
      dplyr::collect()

    preview_tbl <- dplyr::bind_rows(df_head, df_tail)
  } else {
    preview_tbl <- object@value |>
      dplyr::filter(j <= 10L) |>
      dplyr::arrange(i, j) |>
      head(100L) |>
      dplyr::collect()
  }

  # ij indices for printing
  a_i <- match(preview_tbl$i, filter_i)
  a_j <- match(preview_tbl$j, filter_j)

  if (length(a_i) == 0L) {
    a_i <- NULL
  }
  if (length(a_j) == 0L) {
    a_j <- NULL
  }
  a_x <- NULL

  if (length(preview_tbl$x) != 0L) {
    # catch sparse case where if/else: null
    n_digits <- getOption("dbMatrix.digits", default = 7)
    a_x <- format(round(preview_tbl$x, n_digits), nsmall = n_digits)
  }

  # print matrix values
  if (suppress_rows) {
    # suppressed lines: capture, split, then print individually
    # when suppressed, currently hardcoded to show 3 from head and 3 from tail
    a_out <- capture.output(print_array(
      i = a_i,
      j = a_j,
      x = a_x,
      dims = c(length(p_rown), length(p_coln)),
      class = "dense",
      rownames = p_rown
    ))
    writeLines(a_out[1:4])

    dim_col_out <- dim_col - 10L
    dim_row_out <- dim_row - 6L

    if (ncol(object) == 1) {
      sprintf("\n...suppressing %d elements\n\n", dim_row_out) |> cat()
    } else if (dim_col_out < 0) {
      sprintf("\n......suppressing %d rows\n\n", dim_row_out) |> cat()
    } else if (dim_col_out == 0) {
      sprintf("\n...... suppressing %d rows ......\n\n", dim_row_out) |>
        cat()
    } else {
      sprintf(
        "\n......suppressing %d columns and %d rows\n\n",
        dim_col_out,
        dim_row_out
      ) |>
        cat()
    }

    writeLines(a_out[5:7])
  } else {
    # no suppressed lines: Directly print
    print_array(
      i = a_i,
      j = a_j,
      x = a_x,
      dims = c(length(p_rown), length(p_coln)),
      class = "dense",
      rownames = p_rown
    )
  }
})

##  dbSparseMatrix ####
setMethod("show", signature("dbSparseMatrix"), function(object) {
  row_names <- rownames(object)
  col_names <- colnames(object)
  dims <- dim(object)
  dim_row <- dims[1]
  dim_col <- dims[2]

  # print class and dims #
  # -------------------- #

  if (identical(dims, c(0L, 0L))) {
    cat('0 x 0 dbMatrix of class "dbSparseMatrix"\n')
    return()
  } else {
    cat(dim_row, "x", dim_col, ' dbMatrix of class "dbSparseMatrix"\n')
  }

  # preview print #
  # ------------- #

  # print colnames
  colname_show_n <- dim_col - 6L
  if (colname_show_n < 0L) {
    message("[[ Colnames: ", vector_to_string(col_names), " ]]")
  } else if (colname_show_n >= 1L) {
    message(
      "[[ Colnames ",
      vector_to_string(head(col_names, 3L)),
      " ... suppressing ",
      colname_show_n,
      " ...",
      vector_to_string(tail(col_names, 3L)),
      " ]]"
    )
  }

  # get matrix i and j to print
  suppress_rows <- FALSE # flag for whether rows are being suppressed

  # Determine column indices to show (first 10)
  if (dim_col > 10L) {
    filter_j <- seq_len(10L)
    p_coln <- col_names[filter_j]
  } else {
    filter_j <- seq_len(dim_col)
    p_coln <- col_names
  }

  # Determine row indices to show (head 3, tail 3 if large)
  if (dim_row > 6L) {
    filter_i <- c(seq_len(3L), seq(from = dim_row - 2L, to = dim_row))
    p_rown <- row_names[filter_i]
    suppress_rows <- TRUE
  } else {
    filter_i <- seq_len(dim_row)
    p_rown <- row_names
  }

  # prepare subset to print
  if (suppress_rows) {
    # Head query
    df_head <- object@value |>
      dplyr::filter(j <= 10L, i <= 3L) |>
      dplyr::arrange(i, j) |>
      dplyr::collect()

    # Tail query
    df_tail <- object@value |>
      dplyr::filter(j <= 10L, i >= (dim_row - 2L)) |>
      dplyr::arrange(i, j) |>
      dplyr::collect()

    preview_tbl <- dplyr::bind_rows(df_head, df_tail)
  } else {
    preview_tbl <- object@value |>
      dplyr::filter(j <= 10L) |>
      dplyr::arrange(i, j) |>
      head(100L) |>
      dplyr::collect()
  }

  # ij indices for printing
  a_i <- match(preview_tbl$i, filter_i)
  a_j <- match(preview_tbl$j, filter_j)

  if (length(a_i) == 0L) {
    a_i <- NULL
  }
  if (length(a_j) == 0L) {
    a_j <- NULL
  }
  a_x <- NULL

  if (length(preview_tbl$x) != 0L) {
    # catch sparse case where if/else: null
    n_digits <- getOption("dbMatrix.digits", default = 7)
    a_x <- format(round(preview_tbl$x, n_digits), nsmall = n_digits)
  }

  # print matrix values
  if (suppress_rows) {
    # suppressed lines: capture, split, then print individually
    # when suppressed, currently hardcoded to show 3 from head and 3 from tail
    a_out <- capture.output(print_array(
      i = a_i,
      j = a_j,
      x = a_x,
      dims = c(length(p_rown), length(p_coln)),
      class = "sparse",
      rownames = p_rown
    ))
    writeLines(a_out[1:4])

    dim_col_out <- dim_col - 10L
    dim_row_out <- dim_row - 6L

    if (dim_col_out < 0) {
      sprintf("\n......suppressing %d rows\n\n", dim_row_out) |>
        cat()
    } else if (dim_col_out == 0) {
      sprintf("\n.......... suppressing %d rows ..........\n\n", dim_row_out) |>
        cat()
    } else {
      sprintf(
        "\n......suppressing %d columns and %d rows\n\n",
        dim_col_out,
        dim_row_out
      ) |>
        cat()
    }

    writeLines(a_out[5:7])
  } else {
    # no suppressed lines: Directly print
    print_array(
      i = a_i,
      j = a_j,
      x = a_x,
      dims = c(length(p_rown), length(p_coln)),
      class = "sparse",
      rownames = p_rown
    )
  }
})

# constructors ####

#' @title Create a \code{dbSparseMatrix} or \code{dbDenseMatrix} object
#' @description
#' Create an S4 \code{dbMatrix} object in sparse or dense triplet vector format.
#' @param value data to be added to the database. See details for supported data types \code{(required)}
#' @param name table name to assign within database \code{(required, default: "dbMatrix")}
#' @param con DBI or duckdb connection object \code{(required)}
#' @param overwrite whether to overwrite if table already exists in database \code{(required)}
#' @param class class of the dbMatrix: \code{dbDenseMatrix} or \code{dbSparseMatrix} \code{(required)}
#' @param dims dimensions of the matrix \code{(optional: [int, int])}
#' @param dim_names dimension names of the matrix \code{(optional: list(enum, enum))}
#' @param mtx_rowname_file_path path to .mtx rowname file to be read into \code{(optional)}
#' database. by default, no header is assumed.
#' @param mtx_rowname_col_idx column index of row name file \code{(optional)}
#' @param mtx_colname_file_path path to .mtx colname file to be read into
#' database. by default, no header is assumed. \code{(optional)}
#' @param mtx_colname_col_idx column index of column name file \code{(optional)}
#' @param ... additional params to pass to \code{dplyr::copy_to}
#' @details This function reads in data into a pre-existing DuckDB database.
#' Supported \code{value} data types:
#' \itemize{
#'  \item [`Matrix::dgCMatrix-class`] In-memory sparse matrix from the [`Matrix`] package
#'  \item [`Matrix::dgTMatrix-class`] In-memory triplet vector or COO matrix
#'  \item [`matrix`] In-memory dense matrix from base R
#'  \item \code{.mtx} Path to [.mtx](https://math.nist.gov/MatrixMarket/formats.html) file
#'  \item \code{.csv} Path to .csv file
#'  \item `tbl_duckdb_connection` Table in [`duckdb`] database in ijx format from
#'  existing [`dbMatrix`] object. \code{dims} and \code{dim_names} must be
#'  specified if \code{value} is \code{tbl_duckdb_connection}.
#' }
#' @concept dbMatrix
#' @export
#' @examples
#' dgc <- readRDS(system.file("extdata", "dgc.rds", package = "dbMatrix"))
#' con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
#' dbSparse <- dbMatrix(
#'   value = dgc,
#'   con = con,
#'   name = "sparse_matrix",
#'   class = "dbSparseMatrix",
#'   overwrite = TRUE
#' )
#' dbSparse
dbMatrix <- function(
  value,
  class = NULL,
  con = NULL,
  overwrite = FALSE,
  name = "dbMatrix",
  dims = NULL,
  dim_names = NULL,
  mtx_rowname_file_path,
  mtx_rowname_col_idx = 1,
  mtx_colname_file_path,
  mtx_colname_col_idx = 1,
  ...
) {
  # check inputs
  .check_value(value)
  .check_con(con)
  .check_name(name)
  .check_overwrite(
    conn = con,
    overwrite = overwrite,
    name = name,
    skip_value_check = TRUE
  )

  # check class
  if (is.null(class)) {
    stopf("Invalid class: choose 'dbDenseMatrix' or 'dbSparseMatrix'")
  }
  if (
    !is.character(class) | !(class %in% c("dbDenseMatrix", "dbSparseMatrix"))
  ) {
    stopf("Invalid class: choose 'dbDenseMatrix' or 'dbSparseMatrix'")
  }

  # check value and class mismatch
  if (
    (inherits(value, "matrix") | inherits(value, "denseMatrix")) &
      class == "dbSparseMatrix"
  ) {
    stopf("Class mismatch: set class to 'dbDenseMatrix' for dense matrices")
  }
  if (
    (inherits(value, "dgCMatrix") | inherits(value, "sparseMatrix")) &
      class == "dbDenseMatrix"
  ) {
    stopf("Class mismatch: set class to 'dbSparseMatrix' for sparse matrices")
  }

  # check dims, dim_names
  if (inherits(value, "tbl_duckdb_connection")) {
    if (is.null(dims) | is.null(dim_names)) {
      stop(
        "Invalid dims or dim_names: must be provided for tbl_duckdb_connection objects"
      )
    }
  }

  # initialize data (value)
  data <- NULL

  if (inherits(value, "tbl_duckdb_connection")) {
    # data is already in DB
    data <- value
    dims <- dims
    dim_names <- dim_names
  } else {
    # data must be read in
    if (is.character(value)) {
      # read in from file
      if (grepl("\\.csv|\\.tsv|\\.txt", value)) {
        stop("File type not yet supported. Please use .mtx file format.")
        # TODO: implement dense to sparse conversion. Long to wide pivot not yet
        #       supported for out of memory in duckdb.
      } else if (grepl("\\.mtx", value)) {
        data <- readMM(
          con = con,
          value = value,
          name = name,
          overwrite = overwrite
        )

        dims <- get_MM_dim(value)

        dim_names <- get_MM_dimnames(
          mtx_file_path = value,
          mtx_rowname_file_path = mtx_rowname_file_path,
          mtx_rowname_col_idx = mtx_rowname_col_idx,
          mtx_colname_file_path = mtx_colname_file_path,
          mtx_colname_col_idx = mtx_colname_col_idx
        )
      } else {
        stop(
          "Invalid file type. Please provide a .mtx, .csv, .txt, or .tsv file."
        )
      }
    } else if (inherits(value, "matrix") | inherits(value, "Matrix")) {
      return(as.dbMatrix(
        value,
        con = con,
        name = name,
        overwrite = overwrite,
        ...
      ))
    } else {
      stopf('Invalid "value" provided. See ?dbMatrix for help.')
    }
  }

  # Set dimnames if not provided
  if (is.null(unlist(dim_names))) {
    row_names <- as.factor(paste0("row", 1:dims[1]))
    col_names <- as.factor(paste0("col", 1:dims[2]))
    dim_names <- list(row_names, col_names)
  }

  if (class == "dbSparseMatrix") {
    set_class <- "dbSparseMatrix"
  } else if (class == "dbDenseMatrix") {
    set_class <- "dbDenseMatrix"
  } else {
    ## redundant check from above
    stopf("Please specify dbMatrix class: 'dbDenseMatrix' or 'dbSparseMatrix'")
  }

  res <- new(
    Class = set_class,
    value = data,
    name = name,
    init = TRUE,
    dim_names = dim_names,
    dims = dims
  )

  return(res)
}

# converters ####

#' Convert a dbSparseMatrix to dbDenseMatrix
#' @description Internal function to convert a [`dbSparseMatrix`] to
#' [`dbDenseMatrix`].
#' @param x A [`dbSparseMatrix`] object
#' @param chunk_size integer. Number of columns to process per chunk during densification.
#' If NULL (default), the function first checks the global option `dbMatrix.chunk_size`.
#' If that is also NULL, it calculates a chunk size such that the estimated memory usage
#' of each chunk does not exceed `dbMatrix.max_mem_convert` (default 8GB).
#' If the total size is within the limit, a single chunk is used.
#' @return A [`dbDenseMatrix`] object
#' @keywords internal
#' @examples
#' \dontrun{
#' dbsm <- sim_dbSparseMatrix(10, 10)
#' dbdm <- .to_db_dense(dbsm)
#' }
.to_db_dense <- function(x, chunk_size = NULL) {
  if (!inherits(x, "dbSparseMatrix")) {
    stopf("Input must be a dbSparseMatrix object")
  }

  # Guard: Check if densification is allowed
  if (!getOption("dbMatrix.allow_densify", default = FALSE)) {
    stop(
      "Automatic sparse-to-dense conversion is disabled.\n",
      "To enable, review the documentation: ?dbMatrix_options\n",
      "Then set: options(dbMatrix.allow_densify = TRUE)",
      call. = FALSE
    )
  }

  info <- .get_dbMatrix_info(x)
  con <- info$con
  n_cols <- as.integer(info$n_cols)[1]
  n_rows <- as.integer(info$n_rows)[1]
  verbose <- getOption("dbMatrix.verbose", default = TRUE)

  # 1. Hot Path: Use Precomputed Table
  precomp_result <- .find_precompute_table(con, n_rows, n_cols)
  if (!is.null(precomp_result)) {
    precomp_name <- precomp_result$name
    use_transposed <- precomp_result$transposed

    if (verbose) {
      msg <- if (use_transposed) {
        "Using precomputed table '{precomp_name}' for densification (transposed)."
      } else {
        "Using precomputed table '{precomp_name}' for densification."
      }
      cli::cli_alert_info(msg)
    }

    precomp <- dplyr::tbl(con, precomp_name)

    if (use_transposed) {
      precomp <- precomp |>
        dplyr::rename(i_orig = i, j_orig = j) |>
        dplyr::rename(i = j_orig, j = i_orig) |>
        dplyr::filter(i <= !!n_rows, j <= !!n_cols)
    } else {
      precomp <- precomp |>
        dplyr::filter(i <= !!n_rows, j <= !!n_cols)
    }

    x_tbl <- x[]

    if ("idx" %in% colnames(precomp) && !use_transposed) {
      x_tbl <- x_tbl |> dplyr::mutate(idx = (j - 1) * !!n_rows + (i - 1))
      data <- precomp |>
        dplyr::left_join(x_tbl, by = "idx", suffix = c("", ".dbsm"))
    } else {
      data <- precomp |>
        dplyr::left_join(x_tbl, by = c("i", "j"), suffix = c("", ".dbsm"))
    }

    val_col <- if ("x.dbsm" %in% colnames(data)) "x.dbsm" else "x"

    res <- new(
      "dbDenseMatrix",
      value = data |>
        dplyr::transmute(i, j, x = dplyr::coalesce(!!dplyr::sym(val_col), 0)),
      name = unique_table_name(prefix = "tmp_dbDenseMatrix_hot"),
      dims = info$dims,
      dim_names = info$dim_names,
      init = TRUE
    )
    return(res)
  }

  # 2. Cold Path: JIT Densification
  if (verbose) {
    cli::cli_alert_info(
      "Performing on-the-fly densification (cold path). See ?dbMatrix_options for details."
    )
  }

  if (is.null(chunk_size)) {
    chunk_size <- getOption("dbMatrix.chunk_size")
    if (is.null(chunk_size)) {
      limit <- getOption("dbMatrix.max_mem_convert", default = 8 * 1024^3)
      chunk_size <- max(
        1L,
        min(floor(limit / (as.numeric(n_rows) * 8)), n_cols)
      )
    }
  }

  # Cap chunks to avoid parser limits
  MAX_CHUNKS <- getOption("dbMatrix.max_chunks", default = 10000L)
  if (ceiling(n_cols / chunk_size) > MAX_CHUNKS) {
    chunk_size <- ceiling(n_cols / MAX_CHUNKS)
    cli::cli_alert_warning(
      "Adjusted chunk_size to {chunk_size} to limit query complexity."
    )
  }

  col_starts <- seq(1, as.integer(n_cols), by = chunk_size)
  base_sql <- dbplyr::sql_render(x[])

  queries <- lapply(col_starts, function(start) {
    end <- min(start + chunk_size - 1, n_cols)
    grid_sql <- glue::glue(
      "SELECT i, j FROM range(1, {n_rows} + 1) t1(i) CROSS JOIN range({start}, {end} + 1) t2(j)"
    )
    glue::glue(
      "SELECT grid.i, grid.j, COALESCE(CAST(data.x AS DOUBLE), 0.0) as x FROM ({grid_sql}) grid LEFT JOIN ({base_sql}) data ON grid.i = data.i AND grid.j = data.j"
    )
  })

  .combine_tree <- function(qs) {
    if (length(qs) == 0) {
      return(NULL)
    }
    if (length(qs) == 1) {
      return(qs[[1]])
    }
    mid <- floor(length(qs) / 2)
    glue::glue(
      "({.combine_tree(qs[1:mid])}) UNION ALL ({.combine_tree(qs[(mid + 1):length(qs)])})"
    )
  }

  new(
    "dbDenseMatrix",
    value = dplyr::tbl(con, dplyr::sql(.combine_tree(queries))),
    name = NA_character_,
    dims = info$dims,
    dim_names = info$dim_names,
    init = TRUE
  )
}

#' @description
#' Convert a dbDenseMatrix to a dbSparseMatrix on disk using SQL.
#' @param db_dense dbDenseMatrix object to convert to dbSparseMatrix
#' @noRd
#' @keywords internal
.to_db_sparse <- function(db_dense) {
  stopf("Not yet supported")
}

# Create ijx vector representation of sparse matrix, keeping zeros
# Updates dgcmatrix by reference
# Copied from below:
# https://stackoverflow.com/questions/64473488/melting-a-sparse-matrix-dgcmatrix-and-keeping-its-zeros
#' @noRd
get_dense_ijx_dt <- function(x) {
  dplyr::tibble(
    i = rownames(x)[row(x)],
    j = colnames(x)[col(x)],
    x = as.numeric(x)
  )
}

#' to_ijx_disk
#'
#' @param con duckdb connection
#' @param name name of table to convert to ijx on disk
#'
#' @return remote table in long format unpivoted from wide format matrix
#' @keywords internal
to_ijx_disk <- function(con, name) {
  stopf("Not yet supported")
}

## as.matrix ####
#' Convert [`dbMatrix`] to in-memory matrix
#'
#' @param x A [`dbMatrix`] object (dbSparseMatrix or dbDenseMatrix)
#' @param ... Additional arguments (not used)
#' @param sparse Logical indicating if the output should be a sparse matrix \code{default:FALSE}
#' @param names Logical indicating if the output should have dimnames. \code{default:FALSE}
#' @description
#' Converts a [`dbMatrix`] object into an in-memory matrix or sparse matrix.
#'
#' @details
#' This method converts a [`dbMatrix`] object into an in-memory
#' [`Matrix::dgCMatrix-class`] (sparse = TRUE) or `matrix()` (default, sparse = FALSE).
#'
#' **Warning: This function can cause memory issues for large [`dbMatrix`] objects.**
#'
#' Set `sparse = TRUE` to convert to a sparse matrix.
#' Set `names = TRUE` to keep dimnames.
#'
#' @return A [`Matrix::dgCMatrix-class`] or [`matrix`]
#' @export
#' @concept dbMatrix
#' @method as.matrix dbMatrix
#' @export
as.matrix.dbMatrix <- function(x, ..., sparse = FALSE, names = TRUE) {
  dims <- dim(x)
  n_rows <- dims[1]
  n_cols <- dims[2]
  dim_names <- dimnames(x)

  # checks - only block 1x1 (scalar), allow 1xN and Nx1 matrices
  if (all(dims == 1)) {
    stopf("Use `as.vector()` for scalar (1x1) dbMatrix objects")
  }

  if (dims[1] > 1e5 || dims[2] > 1e5) {
    cli::cli_alert_warning(
      "Warning: Converting large dbMatrix to in-memory Matrix."
    )
  }

  if (is(x, "dbDenseMatrix") & sparse) {
    stopf("Cannot convert dbDensematrix into sparse matrix. Set sparse=FALSE")
  }

  if (is(x, "dbSparseMatrix") & !sparse) {
    cli::cli_alert_info(
      "Converting dbSparseMatrix into dense matrix. Set 'sparse=TRUE' to construct 'dgCMatrix'."
    )
  }

  # Chunked dense matrix conversion to avoid memory inflation
  if (!sparse) {
    # Limit max memory usage
    .check_mem_limit(x)

    # Calculate memory limits
    limit <- getOption("dbMatrix.max_mem_convert", default = 8 * 1024^3)
    est_final_size <- as.numeric(n_rows) * as.numeric(n_cols) * 8

    # Intermediate df is ~16 bytes per element (4+4+8)
    # Use 24 to be safe and account for vectors
    est_intermediate_size <- as.numeric(n_rows) * as.numeric(n_cols) * 24
    est_peak_memory <- est_final_size + est_intermediate_size

    # Pre-allocate dense matrix with zeros
    mat <- matrix(0, nrow = n_rows, ncol = n_cols)



    if (est_peak_memory < limit) {
      if (getOption("dbMatrix.verbose", default = TRUE)) {
        cli::cli_alert_info("Using fast in-memory conversion.")
      }

      # Get all sparse data first
      con <- dbplyr::remote_con(x[])
      sql <- dbplyr::sql_render(x[])
      dat <- DBI::dbGetQuery(con, sql)

      # Fill sparse values into dense matrix
      if (nrow(dat) > 0) {
        idx <- (as.integer(dat$j) - 1L) *
          as.numeric(n_rows) +
          as.integer(dat$i)
        mat[idx] <- dat$x
      }
    } else {
      if (getOption("dbMatrix.verbose", default = TRUE)) {
        cli::cli_alert_info(
          "Using chunked streaming conversion to save memory."
        )
      }

      # Single-fetch using collect() is faster than LIMIT/OFFSET chunking
      dat <- dplyr::collect(x[])

      if (nrow(dat) > 0) {
        idx <- (as.integer(dat$j) - 1L) *
          as.numeric(n_rows) +
          as.integer(dat$i)
        mat[idx] <- dat$x
      }
    }

    if (names) {
      dimnames(mat) <- dim_names
    } else {
      dimnames(mat) <- NULL
    }
    return(mat)
  }

  # Stream to disk for sparse matrix (non-OP path)
  temp_file <- tempfile(fileext = ".mtx")

  # Ensure cleanup
  tryCatch(
    {
      writeMM(x, temp_file)
      mat <- Matrix::readMM(temp_file)
      mat <- as(mat, "CsparseMatrix")

      if (names) {
        dimnames(mat) <- dim_names
      }

      return(mat)
    },
    finally = {
      if (file.exists(temp_file)) {
        unlink(temp_file)
      }
    }
  )
}

#' @method as.matrix dbSparseMatrix
#' @export
as.matrix.dbSparseMatrix <- function(x, ...) {
  as.matrix.dbMatrix(x, ...)
}

#' @method as.matrix dbDenseMatrix
#' @export
as.matrix.dbDenseMatrix <- function(x, ...) {
  as.matrix.dbMatrix(x, ...)
}

#' @noRd
#' @keywords internal
#' @param x dbDenseMatrix containing 1 in dim
# NOTE: 1D dbMatrix objects (dim has 1) can be converted to vector
setMethod(
  "as.vector",
  signature(x = "dbDenseMatrix"),
  function(x, mode = "any") {
    if (1 %in% dim(x)) {
      if (dim(x)[1] == 1) {
        out <- x[] |>
          dplyr::select(j, x) |>
          dplyr::arrange(j) |>
          dplyr::collect() |>
          dplyr::pull(x)
        names(out) <- colnames(x)
      } else {
        out <- x[] |>
          dplyr::select(i, x) |>
          dplyr::arrange(i) |>
          dplyr::collect() |>
          dplyr::pull(x)
        names(out) <- rownames(x)
      }
      return(out)
    }

    stopf("Use `as.matrix()` for dbMatrix objects")
  }
)

#' Convert [`Matrix`] to [`dbMatrix`]
#' @description
#' Converts in-memory [`matrix`], [`Matrix::dgeMatrix-class`], or
#' [`Matrix::dgCMatrix-class`] into a [`dbMatrix`] object.
#'
#' @details
#' If no `con` is provided, a temporary in-memory database connection is created.
#' If no `name` is provided, a unique table name is generated.
#'
#' @param x [`matrix`], [`Matrix::dgeMatrix-class`], or [`Matrix::dgCMatrix-class`] \code{required}
#' @param con `tbl_duckdb_connection` \code{default:"memory"} Connection to
#' DuckDB database connection. If not provided, a temporary in-memory
#' DuckDB database is created. \code{':temp:'}  will create a DuckDB database
#' in the temporary directory. \code{':memory:'} will create a DuckDB database
#' in memory.
#' @param name \code{default:"memory"} table name in the database. If not
#' provided, a unique table name is generated.
#' @param ... Additional arguments passed to [`dbMatrix`]
#' @export
#' @concept dbMatrix
as.dbMatrix <- function(x, con, name, ...) {
  # checks
  stopifnot(is(x, "matrix") | is(x, "dgeMatrix") | is(x, "dgCMatrix"))
  if (missing(con) || (is.character(con) && con == ":memory:")) {
    con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    cli::cli_alert_info("Creating in-memory database connection...")
  } else if (is.character(con) && con == ":temp:") {
    con <- DBI::dbConnect(
      duckdb::duckdb(),
      tempfile(
        tmpdir = tempdir(),
        fileext = ".db"
      )
    )
    cli::cli_alert_info("Creating temporary database connection...")
  } else {
    stopifnot(inherits(con, "duckdb_connection"))
  }

  if (missing(name)) {
    name <- unique_table_name("dbMatrix")
  } else {
    stopifnot(is.character(name))
  }

  # Convert matrix into dbMatrix
  if (inherits(x, "matrix") | inherits(x, "dgeMatrix")) {
    class <- "dbDenseMatrix"
  } else if (inherits(x, "dgCMatrix")) {
    class <- "dbSparseMatrix"
  } else {
    stopf("Matrix type not supported")
  }

  res <- dbMatrix(value = x, con = con, name = name, class = class, ...)

  return(res)
}

#' @title as_ijx
#' @param x dgCMatrix or matrix
#' @noRd
as_ijx <- function(x) {
  # check that x is a dgCMatrix, matrix, or dgeMatrix
  stopifnot(
    inherits(x, "dgCMatrix") ||
      inherits(x, "matrix") ||
      inherits(x, "dgeMatrix")
  )

  # Convert dgc into TsparseMatrix class from {Matrix}
  if (is(x, "dgCMatrix")) {
    ijx <- as(x, "TsparseMatrix")
    df <- data.table::data.table(i = ijx@i + 1L, j = ijx@j + 1L, x = ijx@x)
  } else if (is(x, "matrix") | is(x, "dgeMatrix")) {
    row_indices <- rep(seq_len(nrow(x)), times = ncol(x))
    col_indices <- rep(seq_len(ncol(x)), each = nrow(x))
    values <- as.vector(x)
    df <- data.table::data.table(
      i = row_indices,
      j = col_indices,
      x = values
    )
  } else {
    stopf("Matrix type not supported")
  }

  return(df)
}

#' dbMatrix_from_tbl
#' @description Constructs a \code{dbSparseMatrix} object from a \code{tbl_duckdb_connection} object.
#' @details
#' The \code{tbl_duckdb_connection} object must contain dimension names as columns in long format.
#'
#' If \code{value_colName} is provided, the function uses pre-aggregated counts from that column.
#' This is useful when the input table already contains aggregated counts (e.g., from a GROUP BY + SUM operation).
#' If \code{value_colName} is \code{NULL} (default), the function counts occurrences of each row-column pair.
#'
#' @param tbl \code{tbl_duckdb_connection} table in DuckDB database in long format
#' @param con DBI or duckdb connection object \code{(required)}
#' @param rownames_colName \code{character} column name of rownames in tbl \code{(required)}
#' @param colnames_colName \code{character} column name of colnames in tbl \code{(required)}
#' @param value_colName \code{character} column name containing pre-aggregated integer counts.
#' If \code{NULL} (default), counts occurrences of each row-column pair. \code{(optional)}
#' @param name table name to assign within database \code{(required, default: "dbMatrix")}
#' @param overwrite whether to overwrite if table already exists in database \code{(required)}
#'
#' @return `dbMatrix` object
#' @concept dbMatrix
#' @export
dbMatrix_from_tbl <- function(
  tbl,
  rownames_colName,
  colnames_colName,
  value_colName = NULL,
  name = "dbMatrix",
  overwrite = FALSE
) {
  # Check args
  con <- dbplyr::remote_con(tbl)
  .check_con(con)
  .check_tbl(tbl)
  .check_name(name = name)
  .check_overwrite(
    conn = con,
    name = name,
    skip_value_check = TRUE,
    overwrite = overwrite
  )

  if (is.null(rownames_colName) | is.null(colnames_colName)) {
    stop("rownames_colName and colnames_colName must be provided")
  }

  if (!all(c(rownames_colName, colnames_colName) %in% colnames(tbl))) {
    stop(
      "rownames_colName and colnames_colName must be present in tbl colnames"
    )
  }

  if (name %in% DBI::dbListTables(con) & !overwrite) {
    stop(
      "name already exists in the database.
          Please choose a unique name or set overwrite to 'TRUE'."
    )
  }

  # check if i and j are column names
  check_names <- intersect(
    c(
      colnames(tbl),
      as.character(rownames_colName),
      as.character(colnames_colName)
    ),
    c("i", "j")
  )
  if (length(check_names) > 0) {
    stop(
      "i and j are reserved names for matrix dimensions. please choose
         new column names"
    )
  }

  # Validate value_colName if provided
  if (!is.null(value_colName)) {
    if (!value_colName %in% colnames(tbl)) {
      stop(
        "value_colName '",
        value_colName,
        "' not found in tbl. ",
        "Available columns: ",
        paste(colnames(tbl), collapse = ", ")
      )
    }
  }

  rownames_colName <- rlang::sym(rownames_colName)
  colnames_colName <- rlang::sym(colnames_colName)

  # check for NA values in row/col names
  n_na <- tbl |>
    dplyr::filter(is.na(rownames_colName) | is.na(colnames_colName)) |>
    dplyr::tally() |>
    dplyr::pull(n)

  if (n_na > 0) {
    stop("NA values found in rownames or colnames. Please remove NA values.")
  }

  # Aggregate counts based on whether pre-aggregated data is provided
  if (!is.null(value_colName)) {
    # Use pre-aggregated counts from specified column
    value_colName_sym <- rlang::sym(value_colName)

    count_table <- tbl |>
      dplyr::group_by(rownames_colName, colnames_colName) |>
      dplyr::summarise(
        x = sum(!!value_colName_sym, na.rm = TRUE),
        .groups = "drop"
      )

    cli::cli_alert_info(
      "Using pre-aggregated counts from '{value_colName}' column"
    )
  } else {
    # Count occurrences of each row-column pair (original behavior)
    count_table <- tbl |>
      dplyr::group_by(rownames_colName, colnames_colName) |>
      dplyr::summarise(x = dplyr::n(), .groups = "drop")

    cli::cli_alert_info(
      "Counting occurrences of each row-column pair"
    )
  }

  # add label encodings and get dimensions, dim names
  i_encoded <- rlang::sym(paste0(as.character(rownames_colName), "_encoded"))
  j_encoded <- rlang::sym(paste0(as.character(colnames_colName), "_encoded"))

  count_table <- count_table |>
    dplyr::mutate(i_encoded := dplyr::dense_rank(rownames_colName)) |>
    dbplyr::window_order(rownames_colName)

  row_names <- count_table |>
    dplyr::distinct(rownames_colName) |>
    dplyr::arrange(rownames_colName) |>
    dplyr::pull(rownames_colName)

  dim_i <- as.integer(length(row_names))

  count_table <- count_table |>
    dplyr::mutate(j_encoded := dplyr::dense_rank(colnames_colName)) |>
    dbplyr::window_order(colnames_colName) |>
    dplyr::ungroup()

  col_names <- count_table |>
    dplyr::distinct(colnames_colName) |>
    dplyr::arrange(colnames_colName) |>
    dplyr::pull(colnames_colName)

  dim_j <- as.integer(length(col_names))

  ijx <- count_table |>
    dplyr::select(i = i_encoded, j = j_encoded, x) |>
    dplyr::compute(name = name, overwrite = overwrite, temporary = FALSE)

  # set metadata
  dims <- c(dim_i, dim_j)
  dim_names <- list(row_names, col_names)

  res <- new(
    Class = "dbSparseMatrix",
    value = ijx,
    name = name,
    init = TRUE,
    dim_names = dim_names,
    dims = dims
  )

  return(res)
}

# readers ####
#' read_matrix
#' @description Ingest tabular matrix files into database
#' @details
#' Construct a database VIEW of a .csv, .tsv, or .txt files or their .gz/.gzip
#' variants
#' @param value path to .txt, .csv, .tsv or .gzip/.gz variants \code{(required)}
#' @param name name to assign file within database \code{(optional)}.
#' default: "dbMatrix"
#' @param con DBI or duckdb connection object \code{(required)}
#' @param overwrite whether to overwrite if `name` already exists in database.
#' \code{(required)}. default: FALSE
#' @param ... additional params to pass
#'
#' @return tbl_dbi object
#' @noRd
#' @keywords internal
#'
#' @examples
#' print('TODO')
read_matrix <- function(con, value, name = "dbMatrix", overwrite = FALSE, ...) {
  # check inputs
  .check_con(con)
  .check_value(value)
  .check_name(name)
  .check_overwrite(
    conn = con,
    overwrite = overwrite,
    name = name,
    skip_value_check = TRUE
  )

  # Read in files
  if (grepl("\\.csv|\\.tsv|\\.txt", value)) {
    sql <- glue::glue(
      "CREATE OR REPLACE TEMPORARY VIEW {name} AS
       SELECT * FROM read_csv_auto('{value}', header = TRUE);"
    )

    if (!overwrite) {
      sql <- gsub("OR REPLACE ", "", sql, fixed = TRUE)
    }

    DBI::dbExecute(con, sql)

    res <- dplyr::tbl(con, name)
  } else {
    stop("File type not supported.")
  }

  return(res)
}

#' read_MM
#' @description Read matrix market file (.mtx or .mtx.gz)  into database
#' @details
#' Construct a database VIEW or TABLE of a .mtx or .mtx.gz file with columns
#' 'i', 'j', and 'x' representing the row index, column index, and value of
#' the matrix, respectively.
#'
#' By default 'i' and 'j' are of type BIGINT and 'x' is of type DOUBLE.
#' **Note**: lack of support in R for BIGINT may cause errors when pulling data
#' into memory without proper type conversion.
#'
#' By default, .mtx files are expected to contain two lines representing the
#' standard header information.
#' @param value path to .mtx or .mtx.gz file \code{(required)}
#' @param name name to assign file within database \code{(optional)}.
#' default: "dbMatrix"
#' @param con DBI or duckdb connection object \code{(required)}
#' @param overwrite whether to overwrite if `name` already exists in database.
#' \code{(required)}. default: FALSE
#' @param temporary whether to create a temporary view (TRUE) or permanent table (FALSE)
#' \code{(optional)}. default: TRUE
#'
#' @return tbl_dbi object
#' @noRd
#' @keywords internal
#'
#' @examples
#' print("TODO")
readMM <- function(
  con,
  value,
  name = "dbMatrix",
  overwrite = FALSE,
  temporary = TRUE
) {
  # check inputs
  .check_con(con)
  .check_value(value)
  .check_name(name)
  .check_overwrite(
    conn = con,
    overwrite = overwrite,
    name = name,
    skip_value_check = FALSE
  )

  if (grepl("\\.csv|\\.tsv|\\.txt", value)) {
    stop("Please use read_matrix() for .csv, .tsv, .txt files.")
  }

  # Read in .mtx or .mtx.gz file
  if ((grepl("\\.mtx", value))) {
    # Determine if creating temporary view or permanent table
    create_statement <- if (temporary) {
      "CREATE OR REPLACE TEMPORARY VIEW"
    } else {
      "CREATE TABLE"
    }

    # Add IF NOT EXISTS clause if not overwriting
    if (!overwrite && !temporary) {
      create_statement <- paste(create_statement, "IF NOT EXISTS")
    }

    # .mtx reader
    # Note: Using read_csv instead of read_csv_auto for explicit control
    # MTX format: rows starting with % are comments, then a header line with dims,
    # then space-separated triplets (row col value)
    sql <- glue::glue(
      "{create_statement} {name} AS
       SELECT * FROM read_csv(
          '{value}',
          delim = ' ',
          comment = '%',
          columns = {{
              'i': 'BIGINT',
              'j': 'BIGINT',
              'x': 'DOUBLE'
          }},
          header = FALSE,
          skip = 1,
          ignore_errors = FALSE
      );"
    )

    if (!overwrite && temporary) {
      sql <- gsub("OR REPLACE ", "", sql, fixed = TRUE)
    }

    DBI::dbExecute(con, sql)

    res <- dplyr::tbl(con, name)
  } else {
    stop("File type not supported.")
  }

  return(res)
}


#' get_MM_dim
#' @description Internal function to read dimensions of a .mtx file
#' @details
#' Scans for the header of an mtx file (starting with %) and takes one more line
#' representing the dimensions and number of nonzero values.
#'
#' Note: the header size can vary depending on the .mtx file.
#'
#' @param mtx_file_path path to .mtx file to be read into database
#' @return integer vector of dimensions
#' @keywords internal
get_MM_dim <- function(mtx_file_path) {
  if (!file.exists(mtx_file_path)) {
    stop("File does not exist. Check for valid file path.")
  }

  # Read all lines starting with '%' and one additional line representing dims
  # Use gzfile for compressed files, otherwise use file
  if (grepl("\\.gz$", mtx_file_path)) {
    con <- gzfile(mtx_file_path, "r")
  } else {
    con <- file(mtx_file_path, "r")
  }

  header <- character(0)
  repeat {
    line <- readLines(con, n = 1)
    if (length(line) == 0 || !startsWith(line, "%")) {
      break
    }
    header <- c(header, line)
  }
  header <- c(header, line) # Add the dims
  close(con)

  # Extract dimensions from the last line (dims)
  dims <- as.integer(strsplit(header[length(header)], " ")[[1]][1:2])

  return(dims)
}

#' get_MM_dimnames
#' @description Internal function to read row and column names of a .mtx file
#' @details
#' Can be used to read row and column names from .mtx files. Note: these files
#' must not contain a header (colnames).
#'
#' The mtx_rowname_col_idx and mtx_colname_col_idx can be used to specify the column
#' index of the row and column name files, respectively. By default, the first
#' column is used for both.
#'
#' TODO: Support for reading in only rownames or colnames.
#'
#' @param mtx_file_path path to .mtx file to be read into database
#' @param mtx_rowname_file_path path to .mtx rowname file to be read into
#' database. by default, no header is assumed.
#' @param mtx_rowname_col_idx column index of row name file
#' @param mtx_colname_file_path path to .mtx colname file to be read into
#' database. by default, no header is assumed.
#' @param mtx_colname_col_idx column index of column name file
#' @param ... additional params to pass to [data.table::fread()]
#'
#' @return list of row and column name character vectors
#' @keywords internal
get_MM_dimnames <- function(
  mtx_file_path,
  mtx_rowname_file_path,
  mtx_rowname_col_idx = 1,
  mtx_colname_file_path,
  mtx_colname_col_idx = 1,
  ...
) {
  # check inputs
  if (
    !file.exists(mtx_file_path) ||
      !file.exists(mtx_rowname_file_path) ||
      !file.exists(mtx_colname_file_path)
  ) {
    stop("File does not exist. Check for valid file path.")
  }
  if (!is.numeric(mtx_rowname_col_idx) || !is.numeric(mtx_colname_col_idx)) {
    stop("Column index must be an integer.")
  }

  dims <- get_MM_dim(mtx_file_path)

  # Read row and column name files using data.table fread
  rowname_file <- data.table::fread(mtx_rowname_file_path, header = FALSE)
  dim_rownames <- dim(rowname_file)
  colname_file <- data.table::fread(mtx_colname_file_path, header = FALSE)
  dim_colnames <- dim(colname_file)

  # check dimname and column indices
  if (mtx_rowname_col_idx > dim_rownames[2]) {
    stop(
      "'mtx_rowname_col_idx' exceeds number of columns in 'mtx_rowname_file_path'"
    )
  }

  if (mtx_colname_col_idx > dim_colnames[2]) {
    stop(
      "'mtx_colname_col_idx' exceeds number of columns in 'mtx_colname_file_path'"
    )
  }

  # Extract row and column names
  rownames <- rowname_file[, ..mtx_rowname_col_idx][[1]]
  colnames <- colname_file[, ..mtx_colname_col_idx][[1]]

  # Make unique
  # Note: first replicates will be labeled --1, second --2, and so on...
  rownames <- make.unique(rownames, sep = "--")
  colnames <- make.unique(colnames, sep = "--")

  # check for duplicates and matching dimensions
  if (length(unique(rownames)) != dims[1]) {
    stop(
      "Number of unique row names does not match the number of rows in the
         matrix. Check selected col_idx of 'mtx_rowname_file_path' for valid
         row names."
    )
  }

  if (length(unique(colnames)) != dims[2]) {
    stop(
      "Number of unique column names does not match the number of columns in
         the matrix. Check selected col_idx of 'mtx_colname_file_path' for
         valid column names."
    )
  }

  dimnames <- list(rownames, colnames)

  return(dimnames)
}

# dimnames ####
#' Map dimnames to i,j indices
#' @details
#' Constructs a table in a database that contains the accompanying dimnames
#' for a dbMatrix. The resulting columns in the table:
#' * i (row index)
#' * colName_i (rownames),
#' * j (col index)
#' * j_names (colnames)
#' * x (counts of i,j occcurences)
#' @param dbMatrix dbMatrix object
#' @param colName_i name of column rownames to add to database
#' @param colName_j name of column colnames to add to database
#' default: 'FALSE'.'
#' @keywords internal
map_ijx_dimnames <- function(dbMatrix, colName_i, colName_j) {
  # input validation
  .check_name(colName_i)
  .check_name(colName_j)
  con <- get_con(dbMatrix)
  .check_con(con)

  dimnames <- dimnames(dbMatrix)

  # map dimnames to indices in-memory
  dt_rownames <- data.table::data.table(dimnames[[1]])
  data.table::setnames(dt_rownames, colName_i)
  dt_rownames[, i := .I]

  dt_colnames <- data.table::data.table(dimnames[[2]])
  data.table::setnames(dt_colnames, colName_j)
  dt_colnames[, j := .I]

  # register map to db
  duckdb::duckdb_register(con, "temp_rownames", dt_rownames, overwrite = TRUE)
  duckdb::duckdb_register(con, "temp_colnames", dt_colnames, overwrite = TRUE)

  dimnames1_tbl <- dplyr::tbl(con, "temp_rownames")
  dimnames2_tbl <- dplyr::tbl(con, "temp_colnames")

  res <- dbMatrix[] |>
    dplyr::left_join(dimnames1_tbl, by = "i") |>
    dplyr::left_join(dimnames2_tbl, by = "j") |>
    dplyr::select(
      i,
      !!colName_i := colName_i, # !! to unquote
      j,
      !!colName_j := colName_j, # !! to unquote
      x
    )

  return(res)
}

# compute ####
#' @title Force computation of a dbMatrix
#' @description
#' Explicitly compute a dbMatrix and save it to a table in the database.
#' This overrides the default `dplyr::compute` to use a direct `CREATE TABLE AS`
#' statement, which is more robust for large tables in DuckDB.
#'
#' @param x A `dbMatrix` object
#' @param name Name of the table to create. If NULL, a random name is generated.
#' @param temporary Logical. If TRUE (default), create a temporary table.
#' @param dimnames default = TRUE. If TRUE, the rownames and colnames will be
#' saved in the database. This allows full reconstruction of the dbMatrix object
#' using [dbLoad()].
#' @param overwrite Logical. If TRUE, overwrite the table if it already exists.
#' Default is FALSE.
#' @param ... Additional arguments passed to methods (ignored).
#' @return A `dbMatrix` object pointing to the new table.
#' @export
#' @method compute dbMatrix
compute.dbMatrix <- function(
  x,
  name = NULL,
  temporary = TRUE,
  dimnames = TRUE,
  overwrite = FALSE,
  ...
) {
  con <- dbplyr::remote_con(x@value)

  if (is.null(name)) {
    if (!is.na(x@name)) {
      name <- x@name
    } else {
      name <- unique_table_name("dbmatrix_compute")
    }
  }

  # Generate SQL from the lazy tbl
  sql_query <- dbplyr::sql_render(x@value)

  # Note: DuckDB supports CREATE OR REPLACE TEMPORARY TABLE
  temp_str <- if (temporary) "TEMPORARY" else ""
  replace_str <- if (overwrite) "OR REPLACE" else ""

  # We use CREATE OR REPLACE to handle overwrite=TRUE
  full_sql <- glue::glue(
    "CREATE {replace_str} {temp_str} TABLE {name} AS {sql_query}"
  )

  # Execute
  # We wrap in tryCatch to provide better error messages
  tryCatch(
    {
      invisible(DBI::dbExecute(con, full_sql))
    },
    error = function(e) {
      cli::cli_abort("Failed to compute dbMatrix: {e$message}")
    }
  )

  # Write dimnames if requested
  if (dimnames) {
    .write_dimnames(x = x, name = name)
  }

  # Return new dbMatrix pointing to the new table
  new_tbl <- dplyr::tbl(con, name)

  # Update the object
  x@value <- new_tbl
  x@name <- name
  return(x)
}

#' @export
#' @method compute dbSparseMatrix
compute.dbSparseMatrix <- function(
  x,
  name = NULL,
  temporary = TRUE,
  dimnames = TRUE,
  overwrite = FALSE,
  ...
) {
  # First, compute the base matrix using parent method
  con <- dbplyr::remote_con(x@value)

  if (is.null(name)) {
    if (!is.na(x@name)) {
      name <- x@name
    } else {
      name <- unique_table_name("dbmatrix_compute")
    }
  }

  # Generate SQL from the lazy tbl
  sql_query <- dbplyr::sql_render(x@value)

  # Extract temp tables from the query BEFORE materializing
  temp_tables <- .extract_temp_tables(as.character(sql_query))

  temp_str <- if (temporary) "TEMPORARY" else ""
  replace_str <- if (overwrite) "OR REPLACE" else ""

  full_sql <- glue::glue(
    "CREATE {replace_str} {temp_str} TABLE {name} AS {sql_query}"
  )

  tryCatch(
    {
      invisible(DBI::dbExecute(con, full_sql))

      # Clean up temp tables after successful materialization
      if (length(temp_tables) > 0) {
        .cleanup_temp_tables(
          con,
          temp_tables,
          verbose = getOption("dbMatrix.verbose", TRUE)
        )
      }
    },
    error = function(e) {
      cli::cli_abort("Failed to compute dbMatrix: {e$message}")
    }
  )

  # Write dimnames if requested
  if (dimnames) {
    .write_dimnames(x = x, name = name)
  }



  # Return new dbMatrix pointing to the new table
  new_tbl <- dplyr::tbl(con, name)

  # Update the object
  x@value <- new_tbl
  x@name <- name
  return(x)
}

#' @export
#' @method compute dbDenseMatrix
compute.dbDenseMatrix <- compute.dbMatrix
