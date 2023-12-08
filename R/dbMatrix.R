# initialize dbMatrix####

# Method for initializing dbMatrix. Concerns only the processing that is related
# to elements internal to the object.

# For pre-object construction data operations/massaging, see the constructor
# function createDBMatrix()
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

    # check and return #
    # ---------------- #
    validObject(.Object)
    return(.Object)
  }
)

# show ####

##  dbDenseMatrix ####
# Method for initializing dbMatrix. Concerns only the processing that is related
# to elements internal to the object.
setMethod('show', signature(object = 'dbDenseMatrix'), function(object) {
  cat('connection : ', get_dbdir(object), '\n')
  cat('table name : \'', get_tblName(object), '\'\n', sep = '')

  row_names = rownames(object)
  col_names = colnames(object)
  dims = dim(object)
  dim_row = dims[1]
  dim_col = dims[2]

  # print class and dims #
  # -------------------- #

  if(identical(dims,  c(0L, 0L))) {
    cat('0 x 0 matrix of class "dbDenseMatrix"\n')
    return() # exit early if no info
  } else {
    cat(dim_row, 'x', dim_col, ' matrix of class "dbDenseMatrix"\n')
  }

  # preview print #
  # ------------- #

  # print colnames
  colname_show_n = dim_col - 6L
  if(colname_show_n < 0L) {
    message('Colnames: ', vector_to_string(col_names))
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
  # p_coln = head(col_names, 10L)
  p_coln = c(1:3, (dim_col-2):dim_col)
  if(dim_row - 6L > 0L) {
    # p_rown = c(head(row_names, 3L), tail(row_names, 3L))
    p_rown = c(1:3, (dim_row-2):dim_row)
  } else {
    # p_rown = row_names
    p_rown = 1:length(row_names)
  }

  # prepare subset to print
  preview_dt = object@value |>
    dplyr::filter(i %in% p_rown & j %in% p_coln) |>
    data.table::as.data.table()
  data.table::setkeyv(preview_dt, c('i', 'j')) # enforce ordering

  if(nrow(preview_dt) > 0) {
    preview_dt = data.table::dcast(preview_dt, formula = i ~ j, value.var = 'x')
    colnames(preview_dt) = NULL
  } else {
    print("") # TODO update this for sparse matrix
  }

  if(nrow(preview_dt < 7L)) {
    print(preview_dt, digits = 5L, row.names = 'none')
  } else {
    print(preview_dt[1L:3L,], digits = 5L, row.names = 'none')

    sprintf(' ........suppressing %d columns and %d rows\n',
            dim_col - 10L, dim_row - 6L)

    print(preview_dt[4L:6L,], digits = 5L, row.names = 'none')
  }

  cat('\n')

})

##  dbSparseMatrix ####
setMethod('show', signature('dbSparseMatrix'), function(object) {
  cat('connection : ', get_dbdir(object), '\n')
  cat('table name : \'', get_tblName(object), '\'\n', sep = '')

  row_names = rownames(object)
  col_names = colnames(object)
  dims = dim(object)
  dim_row = dims[1]
  dim_col = dims[2]

  # print class and dims #
  # -------------------- #

  if(identical(dims,  c(0L, 0L))) {
    cat('0 x 0 matrix of class "dbSparseMatrix"\n')
    return() # exit early if no info
  } else {
    cat(dim_row, 'x', dim_col, ' matrix of class "dbSparseMatrix"\n')
  }

  # preview print #
  # ------------- #

  # print colnames
  colname_show_n = dim_col - 6L
  if(colname_show_n < 0L) {
    message('Colnames: ', vector_to_string(col_names))
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
    a_x <- preview_tbl$x
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

    sprintf('\n......suppressing %d columns and %d rows\n\n',
            dim_col - 10L, dim_row - 6L) |>
      cat()

    writeLines(a_out[5:7])
  } else {
    # no suppressed lines: Directly print
    print_array(i = a_i, j = a_j, x = a_x, dims = c(length(p_rown), length(p_coln)), rownames = p_rown)
  }


  # if(nrow(preview_dt < 7L)) {
  #   print(preview_dt, digits = 5L, row.names = 'none')
  # } else {
  #   print(preview_dt[1L:3L,], digits = 5L, row.names = 'none')
  #
  #   sprintf(' ........suppressing %d columns and %d rows\n',
  #           object@dims[[2L]] - 10L, dim_row - 6L)
  #
  #   print(preview_dt[4L:6L,], digits = 5L, row.names = 'none')
  # }
  #
  # cat('\n')

})

# constructors ####

# Basic function to generate a dbMatrix obj given input data

#' @title Create a sparse or dense dbMatrix object
#' @name createDBMatrix
#' @description
#' Create an S4 \code{dbMatrix} object in sparse or dense triplet vector format.
#' @param value data to be added to the database. See details for supported data types \code{(required)}
#' @param name table name to assign within database \code{(optional, default: "dbMatrix")}
#' @param db_path path to database on disk (relative or absolute) or in memory \code{(":temp:")}
#' @param overwrite whether to overwrite if table already exists in database \code{(required)}
#' @param class class of the dbMatrix: \code{dbDenseMatrix} or \code{dbSparseMatrix} \code{(required)}
#' @param dims dimensions of the matrix \code{(optional: [int, int])}
#' @param dim_names dimension names of the matrix \code{(optional: list(enum, enum))}
#' @param ... additional params to pass
#' @details This function reads in data into a pre-existing DuckDB database. Based
#' on the \code{name} and \code{db_path} a lazy connection is then made
#' downstream during \code{dbMatrix} initialization.
#'
#' Supported \code{value} data types:
#' \itemize{
#'  \item \code{dgCMatrix} In-memory sparse matrix from the \code{Matrix} package
#'  \item \code{matrix} In-memory dense matrix from base R
#'  \item \code{.mtx} Path to .mtx file (TODO)
#'  \item \code{.csv} Path to .csv file (TODO)
#'  \item \code{tbl_duckdb_connection} Table in DuckDB database in ijx format from
#'  existing \code{dbMatrix} object. \code{dims} and \code{dim_names} must be
#'  specified if \code{value} is \code{tbl_duckdb_connection}.
#' }
#'
#' @export
#' @examples
#' dgc <- dbMatrix:::sim_dgc()
#' dbSparse <- createDBMatrix(value = dgc, db_path = ":temp:",
#'                            name = "sparse_matrix", class = "dbSparseMatrix",
#'                            overwrite = TRUE)
#' dbSparse
createDBMatrix <- function(value,
                           class = NULL,
                           db_path = ":temp:",
                           overwrite = FALSE,
                           name = "dbMatrix",
                           dims = NULL,
                           dim_names = NULL,
                           ...) {
  # check value
  assert_valid_value(value)

  # check db_path
  if (db_path != ":temp:") {
    if (!file.exists(db_path)) {
      stopf("Invalid db_path: file does not exist")
    }
  }

  # check name
  if (!grepl("^[a-zA-Z]", name) | grepl("-", name)) {
    stopf("Invalid name: name must start with a letter and not contain '-'")
  }

  # check class
  if (is.null(class)) {
    stopf("Invalid class: choose 'dbDenseMatrix' or 'dbSparseMatrix'")
  }
  if (!is.character(class) | !(class %in% c("dbDenseMatrix", "dbSparseMatrix"))) {
    stopf("Invalid class: choose 'dbDenseMatrix' or 'dbSparseMatrix'")
  }

  # check value and class mismatch
  if(inherits(value, "matrix") & class == "dbSparseMatrix"){
    stopf("Class mismatch: set class to 'dbDenseMatrix' for dense matrices")
  }
  if(inherits(value, "dgCMatrix") & class == "dbDenseMatrix"){
    stopf("Class mismatch: set class to 'dbSparseMatrix' for sparse matrices")
  }

  # check dims, dim_names
  if (inherits(value, "tbl_duckdb_connection")) {
    if (is.null(dims) | is.null(dim_names)) {
      stopf("Invalid dims or dim_names: must be provided for tbl_duckdb_connection objects")
    }
  }

  # setup db connection
  con <- DBI::dbConnect(duckdb::duckdb(), db_path)

  # initialize data (value)
  data <- NULL

  if (inherits(value, "tbl_duckdb_connection")) { # data is already in DB
    data <- value
    dims <- dims
    dim_names <- dim_names
  } else { # data must be read in
    if (is.character(value)) { # read in from file
      stopf("TODO: read in matrix from file... See read_matrix()")
      # data <- read_matrix(con = con, value = value, name = name,
      #                     overwrite = overwrite, ...)
    } else if(inherits(value, "matrix") | inherits(value, "Matrix")) {
      # convert dense matrix to triplicate vector ijx format
      ijx <- Matrix::summary(as(value, "TsparseMatrix")) |> as.data.frame()

      # write to db
      DBI::dbWriteTable(conn = con, name = name, value = ijx,
                        overwrite = overwrite, ...)

      data <- dplyr::tbl(con, name)

      dims <- dim(value)
      dim_names <- list(rownames(value), colnames(value))
    } else {
      stopf("value must be an in-memory matrix, tbl_duckdb_connection, or
            filepath to matrix data.")
    }
  }

  # 0.0.0.9000 release: hardcode dimnames as enums
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
    stopf("please specify dbMatrix class: 'dbDenseMatrix' or 'dbSparseMatrix'")
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
#' @name toDbDense
#' @description
#' Convert a dbSparseMatrix to a dbDenseMatrix.
#'
#' @param db_sparse dbSparseMatrix object.
#'
#' @examples TODO
#'
#' @export
toDbDense <- function(db_sparse){
  # check if db_dense is a dbDenseMatrix
  if (!inherits(db_sparse, "dbSparseMatrix")) {
    stop("dbSparseMatrix object conversion currently only supported")
  }

  # get connection info
  con <- get_con(db_sparse)

  # get dbm info
  dims <- dim(db_sparse)
  dim_names <- dimnames(db_sparse)
  remote_name <- db_sparse@name
  n_rows <- dims[1]
  n_cols <- dims[2]
  db_path <- get_dbdir(db_sparse)

  # create empty df of all 'i' and 'j' indices
  # note: IN MEMORY operation. may fail for very large matrices
  all_combinations = expand.grid(i = 1:n_rows, j = 1:n_cols, x = 0)

  # write to db
  # note: alternatively create VIEW for in-memory computation (faster but limited by mem)
  dplyr::copy_to(df = all_combinations,
                 name = "dense",
                 dest = con,
                 temporary = TRUE, overwrite = TRUE)

  # # write out all_combinations as a temporary .csv file
  # # note: this is a workaround for the fact that dplyr::copy_to() is slow
  # tictoc::tic()
  # temp_file = tempfile(fileext = ".csv")
  # data.table::fwrite(all_combinations, temp_file)
  #
  # # create table 'all_combinations' in duckdb database connection by reading in .csv temp_file
  # query <- paste0("CREATE TABLE all_combinations AS SELECT * FROM read_csv_auto('", temp_file, "');")
  # DBI::dbExecute(conn = con, statement = query)
  # tictoc::toc()

  # create dense matrix on disk
  # note: time-intensive step
  cat("densifying sparse matrix on disk...")
  sql <- paste0("UPDATE dense ",
                "SET x = ", remote_name, ".x FROM ", remote_name,
                " WHERE dense.i = ", remote_name, ".i AND ",
                "dense.j = ", remote_name, ".j"
                )
  DBI::dbExecute(conn = con, statement = sql)

  # remove old sparse ijx table
  # DBI::dbExecute(conn = con, paste0("DROP TABLE IF EXISTS ", remote_name))

  # rename dense table to existing tbl name
  # rename_sql <- paste("ALTER TABLE all_combinations RENAME TO", remote_name)
  # DBI::dbExecute(conn = con, statement = rename_sql)

  # get new table from database
  data <- dplyr::tbl(con, "dense")

  # Create new dbSparseMatrix object
  db_dense <- new(Class = "dbDenseMatrix",
                  value = data,
                  name = remote_name,
                  dims = dims,
                  dim_names = dim_names,
                  init = TRUE)
  cat("done")
  # show
  db_dense
}

#' @name toDbSparse
#' @description
#' Convert a dbDenseMatrix to a dbSparseMatrix on disk using SQL.
#' @param db_dense dbDenseMatrix object to convert to dbSparseMatrix
#' @noRd
#' @keywords internal
toDbSparse <- function(db_dense){
  stopf("TODO")
  # # check if db_dense is a dbDenseMatrix
  # if (!inherits(db_dense, "dbDenseMatrix")) {
  #   stop("dbDenseMatrix object conversion currently only supported")
  # }
  #
  # # Setup
  # con <- cPool(db_dense)
  # dims <- db_dense@dims
  # remote_name <- db_dense@remote_name
  # n_rows <- dims[1]
  # n_cols <- dims[2]
  #
  # # Create a table with all possible combinations of 'i' and 'j' indices
  # sql <- paste("CREATE TABLE all_indices AS
  #              SELECT i.i, j.j
  #              FROM (SELECT generate_series(1, ?) AS i) AS i
  #              CROSS JOIN (SELECT generate_series(1, ?) AS j) AS j")
  #
  # DBI::dbExecute(
  #   conn = con,
  #   statement = sql,
  #   params = list(n_rows, n_cols),
  #   overwrite = TRUE
  # )
  #
  # # Create a table with all unique 'i' and 'j' indices from
  # # the dbSparseMatrix table
  # sql <- paste("CREATE TABLE unique_indices AS
  #              SELECT DISTINCT i, j
  #              FROM", remote_name)
  #
  # DBI::dbExecute(
  #   conn = con,
  #   statement = sql)
  #
  # # Perform a CROSS JOIN between the unique 'i' and 'j' indices to
  # # create a new table with the missing combinations
  # sql <- paste("CREATE TABLE missing_combinations AS
  #               SELECT i.i, j.j
  #               FROM unique_indices AS i
  #               CROSS JOIN unique_indices AS j
  #               WHERE NOT EXISTS(
  #                 SELECT 1
  #                 FROM ", remote_name, "
  #                 WHERE ", paste0(remote_name, ".i"), "=i.i AND",
  #              paste0(remote_name, ".j"), "= j.j)")
  #
  # DBI::dbExecute(
  #   conn = con,
  #   statement = sql
  # )
  #
  # # Remove the temporary tables
  # DBI::dbRemoveTable(conn = con, name = "all_indices")
  # DBI::dbRemoveTable(conn = con, name = "unique_indices")
  #
  # # Perform a UNION between the dbSparseMatrix table and the new table with missing combinations
  # sql <- paste(
  #   "
  #     CREATE TABLE staged AS
  #     SELECT i, j, x FROM", remote_name, "
  #     UNION ALL
  #     SELECT i, j, 0 AS x FROM missing_combinations
  #     "
  # )
  #
  # DBI::dbExecute(conn = con,
  #                statement = sql)
  #
  # # Remove the temporary tables
  # DBI::dbRemoveTable(conn = con, name = "missing_combinations")
  #
  # # Remove old table
  # DBI::dbExecute(conn = con, paste0("DROP VIEW IF EXISTS ", remote_name))
  #
  # # Rename staged to new remote_name table
  # rename_sql <- paste("ALTER TABLE staged RENAME TO", remote_name)
  # data <- DBI::dbExecute(conn = con, statement = rename_sql)
  #
  # # Create new dbSparseMatrix object
  # db_sparse <- new("dbSparseMatrix",
  #                  data = db_dense@data,
  #                  hash = db_dense@hash,
  #                  remote_name = remote_name,
  #                  dims = dims,
  #                  dim_names = db_dense@dim_names)
  # # show
  # db_sparse
}

# create ijx vector representation of sparse matrix, keeping zeros
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

  # add row idx to ingested matrix
  # TODO: do this without creating a new table
  query <- glue::glue(
    "CREATE TABLE new_table AS SELECT ROW_NUMBER() OVER () AS row_index, * FROM {name};",
    "DROP TABLE {name};",
    "ALTER TABLE new_table RENAME TO {name};"
  )
  invisible(DBI::dbExecute(con, query))

  # create ijx from wide format
  query <- glue::glue("CREATE TABLE ijx AS UNPIVOT {name} ON COLUMNS(* EXCLUDE (row_index));",
                      "DROP TABLE {name};",
                      "ALTER TABLE ijx RENAME TO {name};")
  invisible(DBI::dbExecute(con, query))

  # rename column names
  query <- glue::glue(
    "ALTER TABLE {name} RENAME COLUMN row_index TO i;",
    "ALTER TABLE {name} RENAME COLUMN name TO j;",
    "ALTER TABLE {name} RENAME COLUMN value TO x;",
  )
  invisible(DBI::dbExecute(con, query))

  # remove char from j column
  # TODO: fix the j column data type. still stuck on <chr> after below runs
  query <- glue::glue("UPDATE {name} SET j = CAST(REPLACE(j, 'V', '') AS DOUBLE);")
  invisible(DBI::dbExecute(con, query))

  res <- dplyr::tbl(con, name)

  return(res)
}

# readers ####
read_matrix <- function(con, value, name, overwrite, ...){
  # Notes 11.03.2023
  # - this function is not used in the package
  # - the j column retain <chr> data type even after CASTING to DOUBLE, need to fix this
  # - how do we handle row and col names from matrix files?

  # ingest via duckdb's reader
  if(grepl("\\.csv|\\.tsv|\\.txt", value)) {
    # check if the value is a valid path
    if(!file.exists(value)) {
      stop("File does not exist, please provide a valid file path.")
    }
    if(overwrite){
      query <- paste0("DROP TABLE IF EXISTS ", name)
      DBI::dbExecute(con, query)
    }

    # create new table to connection and overwrite if table is there
    query <- paste0("CREATE TABLE ", name,
                    " AS SELECT * FROM read_csv_auto('", value, "')")
    DBI::dbExecute(con, query)
    data <- dplyr::tbl(con, name)

    # grab col and row names
    query <- glue::glue("DESCRIBE {name};")
    col_names <- DBI::dbGetQuery(con, query) |>
      dplyr::pull("column_name")

    # if all values of col_names start with "V" then set value exists_cnames to FALSE
    if(all(grepl("^V", col_names))) {
      col_names <- col_names
    } else {
      col_names <- NULL
    }

    # check if input is valid (only contains integer or double)
    query <- paste0("SELECT column_name, data_type FROM ",
                    "information_schema.columns WHERE table_name = '",
                    name, "'")
    data_types <- DBI::dbGetQuery(con, query)

    if(!all(data_types$data_type %in% c("BIGINT", "DOUBLE", "INTEGER"))) {
      stop("Input file contains invalid data types,
           please provide a file with only integer or double values.")
    } else {
      # pass to_ijx_disk
      data <- to_ijx_disk(con, name)
    }

  } else {
    # .mtx reader
    if(grepl("\\.mtx", value)) {
      stop("TODO: Read in .mtx file directly into duckdb")
    }
  }

  return(data)
}

