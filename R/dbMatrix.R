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
  } else {
    print("") # TODO update this for sparse matrix
  }

  if(nrow(preview_dt < 7L)) {

    output = as.matrix(preview_dt[1:6,2:7])
    rownames(output) = as.matrix(preview_dt[,1])

    top_left <- format(output[1:3, 1:3], scientific = 2)
    top_right <- format(output[1:3, (ncol(output)-2):ncol(output)],
                        scientific = 2)
    bottom_left <- format(output[(nrow(output)-2):nrow(output), 1:3],
                          scientific = 2)
    bottom_right <- format(output[(nrow(output)-2):nrow(output),
                                  (ncol(output)-2):ncol(output)],
                           scientific = 2)
    ellipsis_row <- crayon::silver(c(rep('⋮', 3),  ".", rep('⋮', 3)))
    ellipsis_col <- crayon::silver(matrix(rep("…", 3), ncol = 1))

    combined <- rbind(
      cbind(top_left, ellipsis_col, top_right),
      ellipsis_row,
      cbind(bottom_left, ellipsis_col, bottom_right)
    )

    rownames(combined) <- crayon::blue(
      c(rownames(output)[1:3],
        "⋮",
        format(rownames(output)[(nrow(output)-2):nrow(output)], scientific = 2)
        )
      )

    colnames(combined) <- crayon::blue(
      c(colnames(output)[1:3],
        "…",
        format(colnames(output)[(ncol(output)-2):ncol(output)], scientific = 2))
      )

    # attempt to add [] to row and col names results in misalignment
    # rownames(combined) <- c(paste0("[", rownames(output)[1:3], ",]"),
    #                         "...",
    #                         paste0("[", rownames(output)[(nrow(output)-2):nrow(output)], ",]")
    #                         )
    #
    # colnames(combined) <- c(paste0("[,", colnames(output)[1:3], "]"),
    #                         "...",
    #                         paste0("[,", colnames(output)[(ncol(output)-2):ncol(output)], "]")
    #                         )

    # # spice it up
    # df_colored <- colorDF::colorDF(as.data.frame(combined))
    #
    # # Set unique color for row names
    # colorDF::df_style(df_colored, "row.names") <- list(
    #   fg = "#e019cc"
    # )
    #
    # # Set unique color for column names
    # colorDF::df_style(df_colored, "col.names") <- list(
    #   fg = "#19e097"
    # )
    #
    # # Remove alternating row coloring by setting interleave to NULL
    # colorDF::df_style(df_colored, "interleave") <- NULL
    #
    # colorDF::print_colorDF(df_colored, header = FALSE, sep = " ")

    write.table(combined,
                quote = FALSE,
                row.names = TRUE,
                col.names = NA,
                sep = "\t",
                file = "")
  } else {
    # data.table::setkey(preview_dt, NULL)
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
#' @param con DBI or duckdb connection object \code{(required)}
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
#'  \item \code{dgTMatrix} In-memory triplet vector or COO matrix
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
#' dbSparse <- createDBMatrix(value = dgc, db_path = ":memory:",
#'                            name = "sparse_matrix", class = "dbSparseMatrix",
#'                            overwrite = TRUE)
#' dbSparse
createDBMatrix <- function(value,
                           class = NULL,
                           con = NULL,
                           overwrite = FALSE,
                           name = "dbMatrix",
                           dims = NULL,
                           dim_names = NULL,
                           ...) {

  # check value
  .check_value(value)

  # check connection object
  if (!DBI::dbIsValid(con) | is.null(con)) {
    stop("Invalid con: con object is not valid or missing.")
  }

  # check name
  if (!grepl("^[a-zA-Z]", name) | grepl("-", name)) {
    stop("Invalid name: name must start with a letter and not contain '-'")
  }

  # check class
  if (is.null(class)) {
    stop("Invalid class: choose 'dbDenseMatrix' or 'dbSparseMatrix'")
  }
  if (!is.character(class) | !(class %in% c("dbDenseMatrix", "dbSparseMatrix"))) {
    stop("Invalid class: choose 'dbDenseMatrix' or 'dbSparseMatrix'")
  }

  # check value and class mismatch
  if(inherits(value, "matrix") & class == "dbSparseMatrix"){
    stop("Class mismatch: set class to 'dbDenseMatrix' for dense matrices")
  }
  if(inherits(value, "dgCMatrix") & class == "dbDenseMatrix"){
    stop("Class mismatch: set class to 'dbSparseMatrix' for sparse matrices")
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
      stopf("TODO: read in matrix from file... See read_matrix()")
      # data <- read_matrix(con = con, value = value, name = name,
      #                     overwrite = overwrite, ...)
    } else if(inherits(value, "matrix") | inherits(value, "Matrix")) {
      # convert dense matrix to triplet vector ijx format
      if(inherits(value, "dgTMatrix")){
        # Convert to 1-based index
        ijx = data.frame(i = value@i + 1, j = value@j + 1, x = value@x)
      } else{
        ijx <- as_ijx(value)
      }

      # write ijx to db
      data <- dplyr::copy_to(dest = con,
                             name = name,
                             df = ijx,
                             overwrite = overwrite,
                             temporary = FALSE)

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
  precompute_mat <- getOption("dbMatrix.precomp", default = NULL)

  if(!is.null(precompute_mat)){

    # if precompute_mat is not atable in con stop throw error
    if(!precompute_mat %in% DBI::dbListTables(con)){
      stop("Precomputed matrix is not a valid table in the connection.")
    }

    precomp <- dplyr::tbl(con, precompute_mat)

    # get the max i value in precomp tbl
    n_rows_pre <- precomp |>
      dplyr::summarize(n_rows = max(i)) |>
      dplyr::collect() |>
      dplyr::pull(n_rows)

    n_cols_pre <- precomp |>
      dplyr::summarize(n_cols = max(j)) |>
      dplyr::collect() |>
      dplyr::pull(n_cols)

    # to prevent 1e4 errors and allow >int32
    n_rows_pre = bit64::as.integer64(n_rows_pre)
    n_cols_pre = bit64::as.integer64(n_cols_pre)

    if(n_rows_pre < n_rows | n_cols_pre < n_cols){
      message <- glue::glue(
        "Precomputed matrix dimensions exceeded. Generate  a larger
        precomputed matrix with at least {n_rows} rows and {n_cols} columns,
        see ?precompute for more details.

        Alternatively, set 'options(dbMatrix.precomp = NULL)' to remove
        the precomputed matrix and densify the dbSparseMatrix on the fly
        (slow, not recommended).
        ")
      stop(message)
    }

    # input validation
    # if(!file.exists(precompute_mat)){
    #   stop("Precomputed matrix file does not exist. Check for valid file path.")
    # }
    # con = get_con(db_sparse)
    # con2 = DBI::dbConnect(duckdb::duckdb(), precompute_mat)
    #
    # precompute_mat = dplyr::tbl(con2, precompute_mat_name)
    #
    # dim_precomp = dim(precompute_mat)[1] * dim(precompute_mat)[2]
    #
    # dim_sparse = dim(db_sparse)[1] * dim(db_sparse)[2]
    #
    # if(dim_precomp < dim_sparse){
    #   stop(paste0("Precomputed matrix is too small.
    #               Download or generate larger precomputed matrix with at least ",
    #               dim(db_sparse)[1], " rows and ",
    #               dim(db_sparse)[2], " columns."))
    # }

    # attach con2 to con of db_sparse
    # db_dense <- dplyr::tbl(db_sparse, precompute_mat)

    # possible errors (and solutions)
    # file doesn't exist (check if path is correct, set again with options(...))
    # precomp matrix dimensionality < db_sparse dimensionality (download or generate larger precomp matrix)
    # precomp matrix is not a valid dbDenseMatrix object (check if precomp matrix is valid dbDenseMatrix object)

    # algo
    # 1. check if precomp matrix exists
    # 2. check if precomp matrix is valid dbDenseMatrix object
    # 3. check if precomp matrix dim satisfies db_sparse-->db_dense dim
    # 4. attach precomp matrix to db_sparse
    # 5. return db_sparse

    key <- precomp |>
      dplyr::filter(i <= n_rows, j <= n_cols) |> # filter out rows and cols that are not in db_sparse
      dplyr::mutate(x = 0)

    data <- key |>
      dplyr::left_join(db_sparse[], by = c("i", "j"), suffix = c("", ".dgc")) |>
      # dplyr::mutate(x = ifelse(is.na(x.dgc), x, x.dgc)) |>
      dplyr::select(-x.dgc)

    # Create new dbSparseMatrix object
    db_dense <- new(Class = "dbDenseMatrix",
                    value = data,
                    name = remote_name,
                    dims = dims,
                    dim_names = dim_names,
                    init = TRUE)

  } else{ # generate dbDenseMatrix from scratch

    warning("Densifying dbSparseMatrix on the fly. See ?precompute to speed up densification.")

    # to prevent 1e4 errors and allow >int32
    n_rows = bit64::as.integer64(n_rows)
    n_cols = bit64::as.integer64(n_cols)

    # Generate i and j vectors from scratch
    sql_i <- glue::glue("SELECT i FROM generate_series(1, {n_rows}) AS t(i)")
    sequence_i <- dplyr::tbl(con, dplyr::sql(sql_i))

    sql_j <- glue::glue("SELECT j FROM generate_series(1, {n_cols}) AS t(j)")
    sequence_j <- dplyr::tbl(con, dplyr::sql(sql_j))

    key <- sequence_i |>
      dplyr::cross_join(sequence_j) |>
      dplyr::mutate(x = 0)

    data <- key |>
      dplyr::left_join(db_sparse[], by = c("i", "j"), suffix = c("", ".dgc")) |>
      dplyr::mutate(x = ifelse(is.na(x.dgc), x, x.dgc)) |>
      dplyr::select(-x.dgc)

    # Create new dbSparseMatrix object
    db_dense <- new(Class = "dbDenseMatrix",
                    value = data,
                    name = remote_name,
                    dims = dims,
                    dim_names = dim_names,
                    init = TRUE)
  }

  # cat("done \n")
  return(db_dense)
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


# save ####
#' Compute or save a dbMatrix to disk
#'
#' @param dbMatrix \code{dbMatrix} object
#' @param name name of table to save to disk
#' @param overwrite whether to overwrite if table already exists in database
#' @param ... additional params to pass
#'
#' @param description
#' Saves dbMatrix table to specified name in the dbMatrix connection.
#' Note: Computing will not persist if the database is in ":memory:".
#'
#' @details
#' This may break functions that rely on the existing table.
#' TODO: sync \code{dbMatrix} constructor with compute
#'
#' @return NULL
#' @keywords internal
#'
#' @examples
#' dbm <- sim_dbSparseMatrix()
#' compute(dbm, "new_table", overwrite = TRUE)
save <- function(dbMatrix, name = '', overwrite = FALSE, ...){
  # input validation
  if(!inherits(dbMatrix, "dbMatrix")){
    stop("Input must be a valid dbMatrix object.")
  }
  con <- get_con(dbMatrix)
  .check_con(conn = con)
  .check_name(name)
  .check_overwrite(conn = con, name = name, overwrite = overwrite)

  # note: this may break relations that rely on the existing tbl
  # idea: check if the table is not computed. if it isn't
  # don't delete it.
  # idea2: create graph of relations and check if the table
  # is a leaf node. if it is, delete it. if it isn't, throw
  # an error. see {dm} package
  if(overwrite & name != ''){ # dplyr::compute still doesn't allow overwrite
    suppressWarnings(x <- dplyr::compute(dbMatrix[], temporary = FALSE))
    temp_name = dbplyr::remote_name(x)

    query <- glue::glue("DROP TABLE IF EXISTS {name}")
    DBI::dbExecute(con, query)

    # rename tbl to name
    query <- glue::glue("ALTER TABLE {temp_name} RENAME TO {name}")
    DBI::dbExecute(con, query)
  } else {
    suppressWarnings(x <- dplyr::compute(dbMatrix[], temporary = FALSE))
    name <- dbplyr::remote_name(x)
  }

  # update dbMatrix
  dbMatrix@value <- dplyr::tbl(con, name)
  dbMatrix@name <- name

  return(dbMatrix)

}
