



# initialize dbMatrix ####

# Method for initializing dbMatrix. Concerns only the processing that is related
# to elements internal to the object.

# For pre-object construction data operations/massaging, see the constructor
# function create_dbMatrix()
#' @keywords internal
#' @noRd
setMethod(
  'initialize',
  signature(.Object = 'dbMatrix'),
  function(.Object, dim_names, dims, ...) {

    # call dbData initialize
    .Object = methods::callNextMethod(.Object, ...)

    # matrix specific data input #
    # -------------------------- #

    # matrix specific
    if(!missing(dim_names)) .Object@dim_names = dim_names
    if(!missing(dims)) .Object@dims = dims


    # default values if no input provided #
    # ----------------------------------- #
    if(is.null(.Object@data)) {

      .Object@dims = c(0L, 0L)
      .Object@dim_names = list(NULL, NULL)
    }

    # check and return #
    # ---------------- #

    validObject(.Object)
    return(.Object)
  })






# Basic function to generate a dbMatrix object given an input to the matrix param.

#' @title Create a matrix with database backend
#' @name createDBMatrix
#' @description
#' Create an S4 dbMatrix object that has a triplet format under the hood (ijx).
#' The data for the matrix is either written to a specified database file or
#' could also be read in from files on disk
#' @param matrix object coercible to matrix or filepath to matrix data accessible
#' by one of the read functions
#' @param remote_name name to assign within database
#' @param db_path path to database on disk
#' @param dim_names list of rownames and colnames of the matrix (optional)
#' @param dims dimensions of the matrix (optional)
#' @param cores number of cores to use if reading into R
#' @param overwrite whether to overwrite if table already exists in database
#' @param callback callback functions to apply to each data chunk before it is
#' sent to the database backend
#' @param ... additional params to pass
#' @include matrix_to_dt.R
#' @export
createDBMatrix = function(matrix,
                          remote_name = 'test',
                          db_path = ':temp:',
                          dim_names,
                          dims,
                          cores = 1L,
                          overwrite = FALSE,
                          callback = callback_formatIJX(),
                          ...) {
  db_path = getDBPath(db_path)
  hash = calculate_backend_id(db_path)
  p = getBackendPool(hash)
  fnq = get_full_table_name_quoted(conn = p, remote_name = remote_name)

  if(existsTableBE(x = p, remote_name = remote_name)) {
    if(isTRUE(overwrite)) {
      DBI::dbRemoveTable(p, DBI::SQL(fnq))
    }
    else {
      stopf(fnq, 'already exists.
          Set overwrite = TRUE to recreate it.')
    }
  }

  # read matrix if needed
  if(is.character(matrix)) {
    # dimnames =
    fstreamToDB(path = matrix,
                backend_ID = hash,
                remote_name = remote_name,
                # indices = c('i', 'j'),
                nlines = 10000L,
                with_pk = FALSE,
                cores = cores,
                callback = callback,
                overwrite = overwrite)
  }

  # convert to IJX format if needed Matrix
  if(inherits(matrix, 'Matrix')) {
    ijx = get_ijx_zero_dt(matrix)
    DBI::dbWriteTable(conn = p,
                      name = remote_name,
                      value = ijx,
                      ...)
  }


  conn = pool::poolCheckout(p)
  on.exit(try(pool::poolReturn(conn), silent = TRUE))

  mtx_tbl = dplyr::tbl(conn, remote_name)
  r_names = mtx_tbl %>% dplyr::distinct(i) %>% dplyr::arrange(i) %>% dplyr::pull()
  c_names = mtx_tbl %>% dplyr::distinct(j) %>% dplyr::arrange(j) %>% dplyr::pull()

  # create table with primary keys in i and j - nonfeasible too large
  # sql_create = create_dbmatrix_sql(hash, full_name_quoted = fnq)
  # DBI::dbExecute(conn, sql_create)
  # pool::poolReturn(conn)
  #
  # ijx %>%
  #   DBI::dbAppendTable(conn = p,
  #                      name = remote_name,
  #                      value = ijx,
  #                      ...)


  dbMat = new('dbMatrix',
              hash = hash,
              remote_name = remote_name,
              dim_names = list(r_names,
                               c_names),
              dims = c(length(r_names),
                       length(c_names)))

  return(dbMat)
}






# compute / DB table creation from lazy dplyr query results ####
# Based on functions in CDMConnector/R/compute.R

#' @name computeDBMatrix
#' @title Execute dplyr query of dbMatrix and save the results in remote database
#' @description
#' Calculate the lazy query of a dbMatrix object and send it to the database backend
#' either as a temporary or permanent table.
#' @param x dbData object to compute from
#' @param remote_name name of table to create on DB
#' @param temporary (default = TRUE) whether to make a temporary table on the DB
#' @param overwrite (default = FALSE) whether to overwrite if remote_name already exists
#' @param ... additional params to pass
#' @export
computeDBMatrix = function(x,
                           remote_name = 'test',
                           temporary = TRUE,
                           overwrite = FALSE,
                           ...) {

  bID = backendID(x)
  p = getBackendPool(backend_ID = bID)
  full_name_quoted = get_full_table_name_quoted(p, remote_name)
  if(existsTableBE(x = p, remote_name = remote_name)) {
    if(isTRUE(overwrite)) {
      DBI::dbRemoveTable(p, DBI::SQL(full_name_quoted))
    }
    else {
      stopf(fullNameQuoted, 'already exists.
          Set overwrite = TRUE to recreate it.')
    }
  }

  args_list = list(x = x,
                   p = p,
                   fnq = full_name_quoted,
                   ...)

  if(isTRUE(temporary)) {
    do.call('compute_temporary', args = args_list)
  } else {
    do.call('compute_dbmatrix_permanent', args = args_list)
  }
}




#' @noRd
compute_dbmatrix_permanent = function(x, p, fnq, ...) {

  conn = pool::poolCheckout(p)
  on.exit(try(pool::poolReturn(conn), silent = TRUE))

  if(dbms(p) %in% c('duckdb', 'oracle')) {
    sql = dbplyr::build_sql(
      con = conn,
      'CREATE TABLE ', fnq, ' (',
      'i VARCHAR,',
      'j VARCHAR,',
      'x DOUBLE,', # will not be computing non numeric matrices
      # 'PRIMARY KEY (i, j)',
      '); ',
      'INSERT INTO ' , fnq, ' (i, j, x) ',
      dbplyr::sql_render(x[], con = conn), ';'
    )
  }

  DBI::dbExecute(conn, sql)
  pool::poolReturn(conn)

  dbMat = new('dbMatrix',
              hash = x@hash,
              remote_name = as.character(fnq),
              dim_names = x@dim_names,
              dims = x@dims)
  return(dbMat)
}





# Internal function to build SQL to create a table for a dbMatrix object with
# columns i j and x. A primary key is defined on columns i and j
#' @param conn DBI connection object, pool, or backend hashID
#' @param full_name_quoted full name of table to create
#' @keywords internal
#' @noRd
create_dbmatrix_sql = function(conn, full_name_quoted) {
  p = evaluate_conn(conn, mode = 'pool')
  conn = pool::poolCheckout(p)
  tryCatch({

    if(dbms(p) == 'duckdb') {
      sql = dbplyr::build_sql(
        'CREATE TABLE ', full_name_quoted, ' (',
        'i VARCHAR,',
        'j VARCHAR,',
        'x DOUBLE,',
        'PRIMARY KEY (i, j)',
        ')',
        con = conn)
    }

    return(sql)

  },
  finally = {
    pool::poolReturn(conn)
  })

}





