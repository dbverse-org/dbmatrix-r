#' @describeIn simulate_objects Simulate a duckdb connection dplyr tbl_Pool in memory
#' @keywords internal
sim_duckdb = function(value = datasets::iris,
                      name = 'test',
                      con = NULL,
                      memory = TRUE) {
  # setup in-memory db if no pool connection provided
  if(is.null(con)) {
    if(memory){
      drv = duckdb::duckdb(dbdir = ':memory:')
    }else{
      # create temporary temp.db file
      temp_file = tempfile(fileext = '.duckdb')
      drv = duckdb::duckdb(dbdir = temp_file)
    }

    # create connection
    con = DBI::dbConnect(drv)
  }

  # check to see if table already exists and if so remove it
  if(DBI::dbExistsTable(con, name)){
    DBI::dbRemoveTable(con, name)
  }

  # add table to database
  DBI::dbWriteTable(con, name, value)

  # show
  dplyr::tbl(con, name)
}

#' @describeIn simulate_objects Simulate a dgcMatrix
#' @title sim_dgc
#' @keywords internal
#' @details
#' This function generates a simulated sparse matrix (dgCMatrix) with number
#' of rows and columns and sets n_vals random values to a non-zero value.
sim_dgc <- function(num_rows = 50, num_cols = 50, n_vals = 50){
  # create simulated dgc matrix
  data <- matrix(0, nrow = num_rows, ncol = num_cols)

  # Set n random values to non-zero
  non_zero_indices <- sample(1:(num_rows*num_cols), n_vals)
  data[non_zero_indices] <- rnorm(num_cols)

  # Create dumby sparse dgc matrix (column-major)
  mat = as(data, "dgCMatrix")

  return(mat)
}

#' @describeIn simulate_objects Simulate a dense matrix
#' @title sim_dgc
#' @keywords internal
#' @details
#' This function generates a simulated dense matrix object with a specified number
#' of rows and columns.
sim_denseMat <- function(num_rows = 50, num_cols = 50){
  # create simulated dense matrix
  mat <- matrix(rnorm(num_rows*num_cols), nrow = num_rows, ncol = num_cols)

  return(mat)
}

#' @describeIn simulate_objects Simulate a duckdb connection dplyr tbl_Pool in memory
#' @title sim_ijx_matrix
#'
#' @details
#' This function generates an ijx representation of a simulated dgCMatrix object
#' with a specified number of rows and columns and sets 50 random values to
#' a non-zero value.
#'
#' @param num_rows The number of rows in the matrix (default: 50)
#' @param num_cols The number of columns in the matrix (default: 50)
#' @param seed_num The seed number for reproducibility (default: 42)
#'
#' @return A dgCMatrix object
#'
#' @examples
#' sim_ijx_matrix()
#'
#' @keywords internal
sim_ijx_matrix = function(mat_type = NULL,
                          num_rows = 50,
                          num_cols = 50,
                          seed_num = 42){
  # check if mat_type is empty
  if (is.null(mat_type)) {
    stop("mat_type must be either 'dense' or 'sparse'")
  }

  # check if num_rows and num_cols are both greater than or equal to 10
  if (num_rows < 10 | num_cols < 10) {
    stop("Number of rows and columns must be at least 10")
  }

  # check if mat_type is either dense or sparse
  if (mat_type != 'dense' & mat_type != 'sparse') {
    stop("mat_type must be either 'dense' or 'sparse'")
  }

  # for reproducibility
  set.seed(seed_num)

  # setup dummy matrix data
  if(mat_type == 'sparse'){

    # Simulate dgcMatrix
    mat = sim_dgc(num_rows = num_rows, num_cols = num_cols)

    # Create sparse ijx representation
    ijx = Matrix::summary(mat)

  } else { # dense matrix
    # Create dumby dataset
    mat = matrix(rnorm(num_rows * num_cols), nrow = num_rows, ncol = num_cols)

    # hardcode dimnames for simulated data
    # row_names = as.factor(paste0("row", 1:num_rows))
    # col_names = as.factor(paste0("col", 1:num_cols))
    row_idx = as.integer(1:num_rows)
    col_idx = as.integer(1:num_cols)

    # set dimnames for matrix
    # rownames(mat) = row_names
    # colnames(mat) = col_names

    # Create dense ijx representation
    ijx <- dplyr::tibble(
      i = row_idx[row(mat)],
      j = col_idx[col(mat)],
      x = as.numeric(mat)
    )

  }

  return(ijx)
}

#' @describeIn simulate_objects Simulate a dbDataFrame in memory
#' @export
sim_dbDataFrame = function(value = NULL, name = 'df_test', key = NA_character_) {
  if(is.null(data)) {
    data = sim_duckdb(name = name)
  }
  if(!inherits(data, 'tbl_sql')) {
    checkmate::assert_class(data, 'data.frame')
    data = sim_duckdb(data = data, name = name)
  }
  dbDataFrame(data = data, remote_name = name, hash = 'ID_dummy',
              init = TRUE, key = key)
}

#' @describeIn simulate_objects Simulate a dbSparseMatrix in memory
#' @description  Simulate a dbSparseMatrix in memory
#' @export
sim_dbSparseMatrix = function(num_rows = 50,
                              num_cols = 50,
                              seed_num = 42,
                              name = 'sparse_test',
                              memory = FALSE) {
  # check input
  if (num_rows < 10 | num_cols < 10) {
    stop("Number of rows and columns must be at least 10.")
  }

  # simulate ijx matrix
  ijx = sim_ijx_matrix(mat_type = 'sparse',
                       num_rows = num_rows,
                       num_cols = num_cols,
                       seed_num = seed_num)

  ijx = ijx |> as.data.frame()

  # add ijx as table to new duckdb connection
  data = sim_duckdb(value = ijx, name = name, memory = memory)

  # save connection
  conn = dbplyr::remote_con(data)

  # setup dimnames
  row_names = as.factor(paste0("row", 1:num_rows))
  col_names = as.factor(paste0("col", 1:num_cols))
  dim_names = list(row_names, col_names)

  # setup dims
  dim = c(as.integer(num_rows), as.integer(num_cols))

  # create dbSparseMatrix obj
  res = createDBMatrix(value = data,
                       name = name,
                       dims = dim,
                       dim_names = dim_names,
                       class = "dbSparseMatrix",
                       init = TRUE)

  # show
  res
}

#' @describeIn simulate_objects Simulate a dbDenseMatrix in memory
#' @description Simulate a dbDenseMatrix in memory.
#' @export
sim_dbDenseMatrix = function(num_rows = 50,
                             num_cols = 50,
                             seed_num = 42,
                             name = 'dense_test',
                             memory = FALSE) {
  # check input
  if (num_rows < 10 | num_cols < 10) {
    stop("Number of rows and columns must be at least 10.")
  }

  # simulate dense matrix
  data = sim_ijx_matrix(mat_type = 'dense',
                        num_rows = num_rows,
                        num_cols = num_cols,
                        seed_num = seed_num)

  # add dense matrix as table to duckdb database
  data = sim_duckdb(value = data, name = name, memory = memory)

  # get connection
  conn = dbplyr::remote_con(data)

  # setup dimnames
  row_names = as.factor(paste0("row", 1:num_rows))
  col_names = as.factor(paste0("col", 1:num_cols))
  # row_names = rownames(data)
  # col_names = colnames(data)
  dim_names = list(row_names, col_names)

  # setup dims
  dim = c(as.integer(num_rows), as.integer(num_cols))

  # create dbDenseMatrix object
  res = createDBMatrix(value = data,
                       name = name,
                       dims = dim,
                       dim_names = dim_names,
                       class = "dbDenseMatrix",
                       init = TRUE)

  # Show
  res
}
