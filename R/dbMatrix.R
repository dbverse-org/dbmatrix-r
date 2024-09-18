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
    .Object = methods::callNextMethod(.Object, ...)

    # matrix specific data input #
    # -------------------------- #
    if(!missing(dim_names)) .Object@dim_names = dim_names
    if(!missing(dims)) .Object@dims = dims


    # default values if no input provided #
    # ----------------------------------- #
    if(is.null(.Object@value)) {
      .Object@dims = c(0L, 0L)
      .Object@dim_names = list(NULL, NULL)
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
setMethod('show', signature('dbDenseMatrix'), function(object) {
  grey_color <- crayon::make_style("grey60")
  # cat(grey_color("# Connection:", get_dbdir(object), "\n"))
  #
  # tbl_name <- dbplyr::remote_name(object[])
  # if(!is.null(tbl_name)){
  #   cat(grey_color("# Name: \'", tbl_name, '\'\n', sep = ''))
  # }

  row_names = rownames(object)
  col_names = colnames(object)
  dims = dim(object)
  dim_row = dims[1]
  dim_col = dims[2]

  # print class and dims #
  # -------------------- #

  if(identical(dims,  c(0L, 0L))) {
    cat('0 x 0 matrix of class "dbDenseMatrix"\n')
    return()
  } else {
    cat(dim_row, 'x', dim_col, ' matrix of class "dbDenseMatrix"\n')
  }

  # preview print #
  # ------------- #

  # print colnames
  colname_show_n = dim_col - 6L
  if(colname_show_n < 0L) {
    message('[[ Colnames: ', vector_to_string(col_names), ' ]]')
  } else if(colname_show_n >= 1L) {
    message(
      '[[ Colnames ',
      vector_to_string(head(col_names, 3L)),
      ' ... suppressing ', colname_show_n, ' ...',
      vector_to_string(tail(col_names, 3L)),
      ' ]]'
    )
  }

  # get matrix i and j to print
  suppress_rows = FALSE # flag for whether rows are being suppressed
  if(dim_col - 10L > 0L) {
    p_coln = c(head(col_names, 10L))
  } else {
    p_coln = col_names
  }
  p_coln = head(col_names, 10L)
  if(dim_row - 6L > 0L) {
    p_rown = c(head(row_names, 3L), tail(row_names, 3L))
    suppress_rows = TRUE
  } else {
    p_rown = row_names
  }

  filter_i = sapply(p_rown, function(f_i) which(f_i == row_names))
  filter_j = sapply(p_coln, function(f_j) which(f_j == col_names))

  # prepare subset to print
  preview_tbl = object@value |>
    dplyr::filter(i %in% filter_i & j %in% filter_j) |>
    dplyr::collect()

  # ij indices for printing
  a_i = sapply(preview_tbl$i, function(i_idx) which(row_names[i_idx] == p_rown))
  a_j = sapply(preview_tbl$j, function(j_idx) which(col_names[j_idx] == p_coln))

  if (length(a_i) == 0L) a_i = NULL
  if (length(a_j) == 0L) a_j = NULL
  a_x <- NULL

  if (length(preview_tbl$x) != 0L) { # catch sparse case where if/else: null
    a_x <- round(preview_tbl$x, digits = 5)
  }

  # print matrix values
  if(suppress_rows) {
    # suppressed lines: capture, split, then print individually
    # when suppressed, currently hardcoded to show 3 from head and 3 from tail
    a_out = capture.output(print_array(i = a_i,
                                       j = a_j,
                                       x = a_x,
                                       dims = c(length(p_rown), length(p_coln)),
                                       rownames = p_rown))
    writeLines(a_out[1:4])

    dim_col_out = dim_col - 10L
    dim_row_out = dim_row - 6L

    if(dim_col_out < 0){
      sprintf('\n......suppressing %d rows\n\n', dim_row_out) |>
        cat()
    } else if (dim_col_out == 0) {
      sprintf('\n...... suppressing %d rows ......\n\n', dim_row_out) |>
        cat()
    }
    else {
      sprintf('\n......suppressing %d columns and %d rows\n\n',
              dim_col_out, dim_row_out) |>
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
      rownames = p_rown
    )
  }

})


##  dbSparseMatrix ####
setMethod('show', signature('dbSparseMatrix'), function(object) {
  grey_color <- crayon::make_style("grey60")
  # cat(grey_color("# Connection:", get_dbdir(object), "\n"))
  # cat(grey_color("# Name: \'", get_tblName(object), '\'\n', sep = ''))

  row_names = rownames(object)
  col_names = colnames(object)
  dims = dim(object)
  dim_row = dims[1]
  dim_col = dims[2]

  # print class and dims #
  # -------------------- #

  if(identical(dims,  c(0L, 0L))) {
    cat('0 x 0 matrix of class "dbSparseMatrix"\n')
    return()
  } else {
    cat(dim_row, 'x', dim_col, ' matrix of class "dbSparseMatrix"\n')
  }

  # preview print #
  # ------------- #

  # print colnames
  colname_show_n = dim_col - 6L
  if(colname_show_n < 0L) {
    message('[[ Colnames: ', vector_to_string(col_names), ' ]]')
  } else if(colname_show_n >= 1L) {
    message(
      '[[ Colnames ',
      vector_to_string(head(col_names, 3L)),
      ' ... suppressing ', colname_show_n, ' ...',
      vector_to_string(tail(col_names, 3L)),
      ' ]]'
    )
  }

  # get matrix i and j to print
  suppress_rows = FALSE # flag for whether rows are being suppressed
  if(dim_col - 10L > 0L) {
    p_coln = c(head(col_names, 10L))
  } else {
    p_coln = col_names
  }
  p_coln = head(col_names, 10L)
  if(dim_row - 6L > 0L) {
    p_rown = c(head(row_names, 3L), tail(row_names, 3L))
    suppress_rows = TRUE
  } else {
    p_rown = row_names
  }

  filter_i = sapply(p_rown, function(f_i) which(f_i == row_names))
  filter_j = sapply(p_coln, function(f_j) which(f_j == col_names))

  # prepare subset to print
  preview_tbl = object@value |>
    dplyr::filter(i %in% filter_i & j %in% filter_j) |>
    dplyr::collect()

  # ij indices for printing
  a_i = sapply(preview_tbl$i, function(i_idx) which(row_names[i_idx] == p_rown))
  a_j = sapply(preview_tbl$j, function(j_idx) which(col_names[j_idx] == p_coln))

  if (length(a_i) == 0L) a_i = NULL
  if (length(a_j) == 0L) a_j = NULL
  a_x <- NULL

  if (length(preview_tbl$x) != 0L) { # catch sparse case where if/else: null
    a_x <- round(preview_tbl$x, digits = 5)
  }

  # print matrix values
  if(suppress_rows) {
    # suppressed lines: capture, split, then print individually
    # when suppressed, currently hardcoded to show 3 from head and 3 from tail
    a_out = capture.output(print_array(i = a_i,
                                       j = a_j,
                                       x = a_x,
                                       dims = c(length(p_rown), length(p_coln)),
                                       rownames = p_rown))
    writeLines(a_out[1:4])

    dim_col_out = dim_col - 10L
    dim_row_out = dim_row - 6L

    if(dim_col_out < 0){
      sprintf('\n......suppressing %d rows\n\n', dim_row_out) |>
        cat()
    } else if (dim_col_out == 0) {
      sprintf('\n.......... suppressing %d rows ..........\n\n', dim_row_out) |>
        cat()
    }
    else {
      sprintf('\n......suppressing %d columns and %d rows\n\n',
              dim_col_out, dim_row_out) |>
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
      rownames = p_rown
    )
  }

})

# constructors ####

#' @title Create a sparse or dense dbMatrix objects
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
#' @param ... additional params to pass
#' @details This function reads in data into a pre-existing DuckDB database.
#' Supported \code{value} data types:
#' \itemize{
#'  \item \code{dgCMatrix} In-memory sparse matrix from the \code{Matrix} package
#'  \item \code{dgTMatrix} In-memory triplet vector or COO matrix
#'  \item \code{matrix} In-memory dense matrix from base R
#'  \item \code{.mtx} Path to .mtx file
#'  \item \code{.csv} Path to .csv file
#'  \item \code{tbl_duckdb_connection} Table in DuckDB database in ijx format from
#'  existing \code{dbMatrix} object. \code{dims} and \code{dim_names} must be
#'  specified if \code{value} is \code{tbl_duckdb_connection}.
#' }
#' @concept dbMatrix
#' @export
#' @examples
#' dgc = readRDS(system.file("data", "dgc.rds", package = "dbMatrix"))
#' con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
#' dbSparse <- dbMatrix(
#'   value = dgc,
#'   con = con,
#'   name = "sparse_matrix",
#'   class = "dbSparseMatrix",
#'   overwrite = TRUE
#' )
#' dbSparse
dbMatrix <- function(value,
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
                     ...) {
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
  if (!is.character(class) | !(class %in% c("dbDenseMatrix", "dbSparseMatrix"))) {
    stopf("Invalid class: choose 'dbDenseMatrix' or 'dbSparseMatrix'")
  }

  # check value and class mismatch
  if((inherits(value, "matrix") | inherits(value, "denseMatrix"))  & class == "dbSparseMatrix"){
    stopf("Class mismatch: set class to 'dbDenseMatrix' for dense matrices")
  }
  if((inherits(value, "dgCMatrix") | inherits(value, "sparseMatrix")) & class == "dbDenseMatrix"){
    stopf("Class mismatch: set class to 'dbSparseMatrix' for sparse matrices")
  }

  # check dims, dim_names
  if (inherits(value, "tbl_duckdb_connection")) {
    if (is.null(dims) | is.null(dim_names)) {
      stop("Invalid dims or dim_names: must be provided for tbl_duckdb_connection objects")
    }
  }

  # initialize data (value)
  data <- NULL

  if (inherits(value, "tbl_duckdb_connection")) { # data is already in DB
    data <- value
    dims <- dims
    dim_names <- dim_names
  } else { # data must be read in
    if (is.character(value)) { # read in from file
      if(grepl("\\.csv|\\.tsv|\\.txt", value)){
        stop("File type not yet supported. Please use .mtx file format.")
        # TODO: implement dense to sparse conversion. Long to wide pivot not yet
        #       supported for out of memory in duckdb.
      } else if(grepl("\\.mtx", value)){
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
        stop("Invalid file type. Please provide a .mtx, .csv, .txt, or .tsv file.")
      }
    } else if(inherits(value, "matrix") | inherits(value, "Matrix")) {
      # convert dense matrix to triplet vector ijx format
      if(inherits(value, "dgTMatrix")){
        # Convert to 1-based index
        ijx = data.frame(i = value@i + 1, j = value@j + 1, x = value@x)
      } else{
        ijx <- as_ijx(value)
      }

      # write ijx to db
      # use duckdb::register instead of dplyr::copy_to ???
      data <- dplyr::copy_to(dest = con,
                             name = name,
                             df = ijx,
                             overwrite = overwrite,
                             temporary = TRUE)

      dims <- dim(value)
      dim_names <- list(rownames(value), colnames(value))
    } else {
      stopf('Invalid "value" provided. See ?dbMatrix for help.')
    }
  }

  # Set dimnames if not provided
  if(is.null(unlist(dim_names))) {
    row_names = as.factor(paste0("row", 1:dims[1]))
    col_names = as.factor(paste0("col", 1:dims[2]))
    dim_names = list(row_names, col_names)
  }

  if(class == "dbSparseMatrix"){
    set_class = "dbSparseMatrix"
  } else if(class == "dbDenseMatrix"){
    set_class = "dbDenseMatrix"
  } else { ## redundant check from above
    stopf("Please specify dbMatrix class: 'dbDenseMatrix' or 'dbSparseMatrix'")
  }

  res <- new(Class = set_class,
             value = data,
             name = name,
             init = TRUE,
             dim_names = dim_names,
             dims = dims)

  return(res)
}

# converters ####

#' @title Convert dbSparseMatrix to dbDenseMatrix
#' @description
#' Convert a dbSparseMatrix to a dbDenseMatrix.
#'
#' @param db_sparse dbSparseMatrix object.
#'
#' @noRd
#'
#' @keywords internal
toDbDense <- function(db_sparse){
  # check if db_dense is a dbDenseMatrix
  if (!inherits(db_sparse, "dbSparseMatrix")) {
    stop("dbSparseMatrix object conversion currently only supported")
  }

  # get dbm info
  con <- get_con(db_sparse)
  dims <- dim(db_sparse)
  dim_names <- dimnames(db_sparse)
  remote_name <- db_sparse@name
  n_rows <- dims[1]
  n_cols <- dims[2]

  db_path <- get_dbdir(db_sparse)

  # see ?dbMatrix::precompute() for more details
  precompute_name <- getOption("dbMatrix.precomp", default = NULL)

  if(!is.null(precompute_name) && (precompute_name %in% DBI::dbListTables(con))){

    precomp <- dplyr::tbl(con, precompute_name)

    # get the max i value in precomp tbl
    n_rows_pre <- precomp |>
      dplyr::summarize(n_rows = max(i)) |>
      dplyr::pull(n_rows)

    n_cols_pre <- precomp |>
      dplyr::summarize(n_cols = max(j)) |>
      dplyr::pull(n_cols)

    # to prevent 1e4 errors and allow >int32
    n_rows_pre <- bit64::as.integer64(n_rows_pre)
    n_cols_pre <- bit64::as.integer64(n_cols_pre)

    if(n_rows_pre < n_rows || n_cols_pre < n_cols){
      cli::cli_alert_warning(
        "Generating a larger precomputed dbMatrix with {n_rows} rows and {n_cols} columns,
        see ?precompute for more details. \n
        ")
      uni_name <- unique_table_name(prefix = "precomp")
      precomp <- precompute(
        conn = con,
        m = n_rows,
        n = n_cols,
        name = uni_name
      )
    }

  } else{ # generate dbDenseMatrix from scratch

    cli::cli_alert_info(paste(
      "Densifying 'dbSparseMatrix' on the fly...",
      sep = "\n",
      collapse = ""
    ))

    # to prevent 1e4 errors and allow >int32
    n_rows <- bit64::as.integer64(n_rows)
    n_cols <- bit64::as.integer64(n_cols)

    # precompute the matrix
    uni_name <- unique_table_name(prefix = "precomp")
    precomp <- precompute(
      conn = con,
      m = n_rows,
      n = n_cols,
      name = uni_name,
      verbose = FALSE
    )
  }

  key <- precomp |>
    dplyr::filter(i <= n_rows, j <= n_cols) |> # filter out rows and cols that are not in db_sparse
    dplyr::mutate(x = 0)

  data <- key |>
    dplyr::left_join(db_sparse[], by = c("i", "j"), suffix = c("", ".dgc")) |>
    dplyr::mutate(x = ifelse(is.na(x.dgc), x, x.dgc)) |>
    dplyr::select(-x.dgc)

  # Create new dbMatrix object
  db_dense <- new(
    Class = "dbDenseMatrix",
    value = data,
    name = remote_name,
    dims = dims,
    dim_names = dim_names,
    init = TRUE
  )

  # cat("done \n")
  return(db_dense)
}

#' @description
#' Convert a dbDenseMatrix to a dbSparseMatrix on disk using SQL.
#' @param db_dense dbDenseMatrix object to convert to dbSparseMatrix
#' @noRd
#' @keywords internal
toDbSparse <- function(db_dense){
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
to_ijx_disk <- function(con, name){
  stopf("Not yet supported")

  # # add row idx to ingested matrix
  # # TODO: do this without creating a new table
  # query <- glue::glue(
  #   "CREATE TABLE new_table AS SELECT ROW_NUMBER() OVER () AS row_index, * FROM {name};",
  #   "DROP TABLE {name};",
  #   "ALTER TABLE new_table RENAME TO {name};"
  # )
  # invisible(DBI::dbExecute(con, query))
  #
  # # create ijx from wide format
  # query <- glue::glue("CREATE TABLE ijx AS UNPIVOT {name} ON COLUMNS(* EXCLUDE (row_index));",
  #                     "DROP TABLE {name};",
  #                     "ALTER TABLE ijx RENAME TO {name};")
  # invisible(DBI::dbExecute(con, query))
  #
  # # rename column names
  # query <- glue::glue(
  #   "ALTER TABLE {name} RENAME COLUMN row_index TO i;",
  #   "ALTER TABLE {name} RENAME COLUMN name TO j;",
  #   "ALTER TABLE {name} RENAME COLUMN value TO x;",
  # )
  # invisible(DBI::dbExecute(con, query))
  #
  # # remove char from j column
  # # TODO: fix the j column data type. still stuck on <chr> after below runs
  # query <- glue::glue("UPDATE {name} SET j = CAST(REPLACE(j, 'V', '') AS DOUBLE);")
  # invisible(DBI::dbExecute(con, query))
  #
  # res <- dplyr::tbl(con, name)
  #
  # return(res)
}

#' as_matrix
#'
#' @param x dbSparseMatrix
#' @param output "matrix"
#' @description
#' Set output to matrix to cast dbSparseMatrix into matrix
#'
#' @details
#' this is a helper function to convert dbMatrix to dgCMatrix or matrix
#' Warning: can cause memory issues if the input matrix is large
#'
#'
#' @return dgCMatrix or matrix
#' @noRd
as_matrix <- function(x, output){
  con <- dbplyr::remote_con(x[])
  .check_con(con)
  dims <- dim(x)
  n_rows <- dims[1]
  n_cols <- dims[2]
  dim_names <- dimnames(x)

  # convert db table into in-memory dt
  if (dims[1] > 1e5 || dims[2] > 1e5){
    cli::cli_alert_warning(
      "Warning: Converting large dbMatrix to in-memory Matrix.")
  }

  if(class(x) == "dbSparseMatrix"){
    max_i <- x[] |> dplyr::summarise(max_i = max(i)) |> dplyr::pull(max_i)
    max_j <- x[] |> dplyr::summarise(max_j = max(j)) |> dplyr::pull(max_j)

    temp_file <- tempfile(tmpdir = getwd(), fileext = ".parquet")

    if (max_i == n_rows & max_j == n_cols){
      x[] |>
        arrow::to_arrow() |>
        arrow::write_parquet(temp_file)
    } else {
      # Generate i and j vectors from scratch
      sql_i <- glue::glue("SELECT i FROM generate_series(1, {n_rows}) AS t(i)")
      sequence_i <- dplyr::tbl(con, dplyr::sql(sql_i))

      sql_j <- glue::glue("SELECT j FROM generate_series(1, {n_cols}) AS t(j)")
      sequence_j <- dplyr::tbl(con, dplyr::sql(sql_j))

      key <- sequence_i |>
        dplyr::cross_join(sequence_j) |>
        dplyr::mutate(x = 0)

      # TODO: skip arrow conversion and write straight to parquet?
      key |>
        dplyr::left_join(x[], by = c("i", "j"), suffix = c("", ".dgc")) |>
        dplyr::mutate(x = ifelse(is.na(x.dgc), x, x.dgc)) |>
        dplyr::select(-x.dgc) |>
        arrow::to_arrow() |>
        arrow::write_parquet(temp_file)
    }

    # Create mat
    dt <- arrow::read_parquet(temp_file)
    mat <- Matrix::sparseMatrix(i = dt$i , j = dt$j , x = dt$x, index1 = TRUE)
    mat <- Matrix::drop0(mat)
    dimnames(mat) = dim_names
    dim(mat) = dims
    unlink(temp_file, recursive = TRUE, force = TRUE)
    if(!missing(output) && output == "matrix"){
      mat <- as.matrix(mat)
    }
  }
  else if(class(x) == "dbDenseMatrix"){
    # Create a temporary file to store the matrix
    temp_file <- tempfile(tmpdir = getwd(), fileext = ".parquet")

    # TODO: skip arrow conversion and write straight to parquet?
    x[] |>
      arrow::to_arrow() |>
      arrow::write_parquet(temp_file)

    dt <- arrow::read_parquet(temp_file)

    # Create a sparse matrix
    mat <- Matrix::sparseMatrix(
      i = dt$i,
      j = dt$j,
      x = dt$x,
      index1 = TRUE
    )
    dimnames(mat) = dim_names
    dim(mat) = dims

    # Convert sparse matrix to dense matrix in-memory
    mat <- as.matrix(mat)

    # Clean up temp files
    unlink(temp_file, recursive = TRUE, force = TRUE)
  }

  return(mat)
}

#' @title as_ijx
#' @param x dgCMatrix or matrix
#' @noRd
as_ijx <- function(x){
  # check that x is a dgCMatrix, matrix, or dgeMatrix
  stopifnot(inherits(x, "dgCMatrix") || inherits(x, "matrix") || inherits(x, "dgeMatrix"))

  # Convert dgc into TsparseMatrix class from {Matrix}
  ijx <- as(x, "TsparseMatrix")

  # Get dbMatrix in triplet vector format (TSparseMatrix)
  # Convert to 1-based indexing
  df = data.table::data.table(i = ijx@i + 1, j = ijx@j + 1, x = ijx@x)

  return(df)
}

#' dbMatrix_from_tbl
#' @description Construcst a \code{dbSparseMatrix} object from a \code{tbl_duckdb_connection} object.
#' @details
#' The \code{tbl_duckdb_connection} object must contain dimension names.
#' @param tbl \code{tbl_duckdb_connection} table in DuckDB database in long format
#' @param con DBI or duckdb connection object \code{(required)}
#' @param rownames_colName \code{character} column name of rownames in tbl \code{(required)}
#' @param colnames_colName \code{character} column name of colnames in tbl \code{(required)}
#' @param name table name to assign within database \code{(required, default: "dbMatrix")}
#' @param overwrite whether to overwrite if table already exists in database \code{(required)}
#'
#' @return `dbMatrix` object
#' @keywords internal
dbMatrix_from_tbl <- function(tbl,
                              rownames_colName,
                              colnames_colName,
                              name = "dbMatrix",
                              overwrite = FALSE){
  # Check args
  con = dbplyr::remote_con(tbl)
  .check_con(con)
  .check_tbl(tbl)
  .check_name(name = name)
  .check_overwrite(
    conn = con,
    name = name,
    skip_value_check = TRUE,
    overwrite = overwrite
  )

  if(is.null(rownames_colName) | is.null(colnames_colName)){
    stop("rownames_colName and colnames_colName must be provided")
  }

  if(!all(c(rownames_colName, colnames_colName) %in% colnames(tbl))){
    stop("rownames_colName and colnames_colName must be present in tbl colnames")
  }

  if(name %in% DBI::dbListTables(con) & !overwrite){
    stop("name already exists in the database.
          Please choose a unique name or set overwrite to 'TRUE'.")
  }

  # check if i and j are column names
  check_names = intersect(c(colnames(tbl), as.character(rownames_colName),
                            as.character(colnames_colName)),
                          c("i", "j"))
  if(length(check_names)>0) {
    stop("i and j are reserved names for matrix dimensions. please choose
         new column names")
  }

  rownames_colName = rlang::sym(rownames_colName)
  colnames_colName = rlang::sym(colnames_colName)

  # check for NA values in row/col names
  n_na <- tbl |>
    dplyr::filter(is.na(rownames_colName) | is.na(colnames_colName)) |>
    dplyr::tally() |>
    dplyr::pull(n)

  if(n_na > 0){
    stop("NA values found in rownames or colnames. Please remove NA values.")
  }

  # summarize the number of counts for each gene per cell id
  count_table <- tbl |>
    dplyr::group_by(rownames_colName, colnames_colName) |>
    dplyr::summarise(x = dplyr::n(), .groups = "drop")

  # add label encodings and get dimensions, dim names
  i_encoded = rlang::sym(paste0(as.character(rownames_colName), "_encoded"))
  j_encoded = rlang::sym(paste0(as.character(colnames_colName), "_encoded"))

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
  dims = c(dim_i, dim_j)
  dim_names = list(row_names, col_names)

  res <- new(Class = "dbSparseMatrix",
             value = ijx,
             name = name,
             init = TRUE,
             dim_names = dim_names,
             dims = dims)

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
read_matrix <- function(con,
                        value,
                        name = "dbMatrix",
                        overwrite = FALSE,
                        ...) {
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
  if(grepl("\\.csv|\\.tsv|\\.txt", value)) {

    sql <- glue::glue(
      "CREATE OR REPLACE VIEW {name} AS
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
#' Construct a database VIEW of a .mtx or .mtx.gz file with columns 'i', 'j',
#' and 'x' representing the row index, column index, and value of the matrix,
#' respectively.
#'
#' By default 'i' and 'j' are of type BIGINT and 'x' is of type DOUBLE.
#' Note: lack of support in R for BIGINT may cause errors when pulling data
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
#' @param ... additional params to pass
#'
#' @return tbl_dbi object
#' @noRd
#' @keywords internal
#'
#' @examples
#' print('TODO')
readMM <- function(con,
                   value,
                   name = "dbMatrix",
                   overwrite = FALSE,
                   ...) {
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

  if(grepl("\\.csv|\\.tsv|\\.txt", value)){
    stop("Please use read_matrix() for .csv, .tsv, .txt files.")
  }

  # Read in .mtx or .mtx.gz file
  if((grepl("\\.mtx", value))) {
    # .mtx reader
    sql <- glue::glue(
      "CREATE OR REPLACE VIEW {name} AS
       SELECT * FROM read_csv_auto(
          '{value}',
          sep = ' ',
          skip = 2,
          columns = {{
              'i': 'BIGINT',
              'j': 'BIGINT',
              'x': 'DOUBLE'
          }},
          header = TRUE
      );"
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
  con <- file(mtx_file_path, "r")
  header <- character(0)
  while (TRUE) {
    line <- readLines(con, n = 1)
    if (length(line) == 0 || !startsWith(line, "%")) {
      break
    }
    header <- c(header, line)
  }
  header <- c(header, line)  # Add the dims
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
#' @param ... additional params to pass to \link{data.table::fread}
#'
#' @return list of row and column name character vectors
#' @keywords internal
get_MM_dimnames <- function(mtx_file_path,
                            mtx_rowname_file_path,
                            mtx_rowname_col_idx = 1,
                            mtx_colname_file_path,
                            mtx_colname_col_idx = 1,
                            ...){

  # check inputs
  if (!file.exists(mtx_file_path) ||
      !file.exists(mtx_rowname_file_path) ||
      !file.exists(mtx_colname_file_path)) {
    stop("File does not exist. Check for valid file path.")
  }
  if (!is.numeric(mtx_rowname_col_idx) || !is.numeric(mtx_colname_col_idx)) {
    stop("Column index must be an integer.")
  }

  dims = get_MM_dim(mtx_file_path)

  # Read row and column name files using data.table fread
  rowname_file <- data.table::fread(mtx_rowname_file_path, header = FALSE)
  dim_rownames = dim(rowname_file)
  colname_file <- data.table::fread(mtx_colname_file_path, header = FALSE)
  dim_colnames = dim(colname_file)

  # check dimname and column indices
  if(mtx_rowname_col_idx > dim_rownames[2]){
    stop("'mtx_rowname_col_idx' exceeds number of columns in 'mtx_rowname_file_path'")
  }

  if(mtx_colname_col_idx > dim_colnames[2]){
    stop("'mtx_colname_col_idx' exceeds number of columns in 'mtx_colname_file_path'")
  }

  # Extract row and column names
  rownames = rowname_file[ , ..mtx_rowname_col_idx][[1]]
  colnames = colname_file[ , ..mtx_colname_col_idx][[1]]

  # Make unique
  # Note: first replicates will be labeled --1, second --2, and so on...
  rownames <- make.unique(rownames, sep = "--")
  colnames <- make.unique(colnames, sep = "--")

  # check for duplicates and matching dimensions
  if(length(unique(rownames)) != dims[1]){
    stop("Number of unique row names does not match the number of rows in the
         matrix. Check selected col_idx of 'mtx_rowname_file_path' for valid
         row names.")
  }

  if(length(unique(colnames)) != dims[2]){
    stop("Number of unique column names does not match the number of columns in
         the matrix. Check selected col_idx of 'mtx_colname_file_path' for
         valid column names.")
  }

  dimnames = list(rownames, colnames)

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
map_ijx_dimnames <- function(dbMatrix,
                             colName_i,
                             colName_j) {
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
    dplyr::select(i,
                  !!colName_i := colName_i, # !! to unquote
                  j,
                  !!colName_j := colName_j, # !! to unquote
                  x)

  return(res)

}





























































