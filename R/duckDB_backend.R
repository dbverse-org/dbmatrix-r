

# initialize backendInfo ####

#' @keywords internal
#' @noRd
setMethod('initialize', signature(.Object = 'backendInfo'),
          function(.Object, driver_call, db_path, ...) {

            if(!missing(driver_call)) .Object@driver_call = driver_call
            if(!missing(db_path)) .Object@db_path = db_path

            if(!is.na(.Object@db_path)) {
              .Object@hash = calculate_hash(.Object@db_path)
            }

            validObject(.Object)
            return(.Object)

          })





# initialize dbData ####
# This virtual class deals with establishment of a connection to the backend
# environment containing the source copy of the connection pool object. IT
# gets that connection pool object and sets up a dplyr lazy table to
# represent the information
# NOTE: DB table writing is handled externally to this initialize function.
# see the relevant create_ functions
setMethod('initialize', signature(.Object = 'dbData'),
          function(.Object, data, hash, remote_name, ...) {

            # data input
            if(!missing(remote_name)) .Object@remote_name = remote_name
            if(!missing(hash)) .Object@hash = hash
            if(!missing(data)) .Object@data = data

            # try to generate if lazy table does not exist
            # the hash and remote_name are needed for this
            if(is.null(.Object@data)) {
              if(!is.na(.Object@hash) & !is.na(.Object@remote_name)) {
                p = getBackendPool(hash = .Object@hash)
                tBE = tableBE(cPool = p, remote_name = remote_name)
                .Object@data = tBE
              }
            }

            if(!inherits(.Object@data, 'tbl_Pool')) {
              stopf('data slot only accepts dplyr class "tbl_Pool"')
            }

            validObject(.Object)
            return(.Object)
          })






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

    .Object = callNextMethod(.Object, ...)


    # matrix specific data input #
    # -------------------------- #

    # matrix specific
    if(!missing(dim_names)) .Object@dim_names = dim_names
    if(!missing(dims)) .Object@dims = dims


    # generate default DB connection if no input provided #
    # --------------------------------------------------- #
    if(is.null(.Object@data)) {
      # test_table = data.table::data.table(i = 'a', j = 'b', x = NA_integer_) # TODO update if matrix uses integer indexing
      #
      # con = DBI::dbConnect(drv = eval(.Object@dvr_call), dbdir = .Object@path)
      # DBI::dbCreateTable(conn = con,
      #                    name = .Object@remote_name,
      #                    fields = test_table)
      # .Object@data = dplyr::tbl(con, .Object@remote_name)
      #
      # .Object@dim = c(1L, 1L)
      # .Object@dim_names = list('a', 'b')
      .Object@dims = c(0L, 0L)
      .Object@dim_names = list(NULL, NULL)
    }



    # check and return #
    # ---------------- #

    validObject(.Object)
    return(.Object)
  })













# Basic function to generate a dbMatrix object given an input to the matrix param.
# boot_calls are for reconnecting any connections that are not directly from
# a file. One example is data loaded in via arrow

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
#' @param to_triplet (default = TRUE) conversion to triplet (ijx) format
#' @param dim_names list of rownames and colnames of the matrix (optional)
#' @param dims dimensions of the matrix (optional)
#' @param cores number of cores to use if reading into R
#' @param ... additional params to pass
#' @include matrix_to_dt.R
#' @noRd
createDBMatrix = function(matrix,
                          remote_name = 'test',
                          db_path = ':temp:',
                          to_triplet = TRUE,
                          dim_names,
                          dims,
                          cores = 1L,
                          overwrite = FALSE,
                          ...) {
  db_path = set_db_path(db_path)
  hash = calculate_hash(db_path)
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
    matrix = readMatrixFlat(matrix)
  }

  # coerce to matrix if not matrix or DB
  if(!inherits(matrix, c('matrix', 'Matrix', 'tbl_dbi'))) {
    matrix = as.matrix(matrix)
  }

  # convert to IJX format if needed
  # TODO update to allow usage of representations with no zeroes
  if(isTRUE(to_triplet)) {
    ijx = get_ijx_zero_dt(matrix)
  }


  conn = pool::poolCheckout(p)
  on.exit(try(pool::poolReturn(conn), silent = TRUE))
  # create table with primary keys in i and j
  sql_create = create_dbmatrix_sql(hash, full_name_quoted = fnq)
  DBI::dbExecute(conn, sql_create)
  pool::poolReturn(conn)

  ijx %>%
    DBI::dbAppendTable(conn = p,
                       name = remote_name,
                       value = ijx,
                       ...)
    # dplyr::copy_to(dest = p,
    #                name = remote_name,
    #                temporary = FALSE,
    #                indexes = list("i", "j"),
    #                ...)
  # DBI::dbWriteTable(conn = conn,
  #                   name = remote_name,
  #                   value = ijx,
  #                   ...)


  dbMat = new('dbMatrix',
              hash = hash,
              remote_name = remote_name,
              dim_names = list(rownames(matrix),
                               colnames(matrix)),
              dims = dim(matrix))

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

  hash = hashBE(x)
  p = try(getBackendPool(hash = hash))
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
    do.call('compute_permanent', args = args_list)
  }
}


#' @noRd
compute_temporary = function(x, p, fnq, ...) {
  x[] = x[] %>%
    dbplyr::compute(name = as.character(fnq),
                    temporary = TRUE,
                    ...)
  x@remote_name = as.character(fnq)

  return(x)
}

#' @noRd
compute_permanent = function(x, p, fnq, ...) {

  conn = pool::poolCheckout(p)
  on.exit(try(pool::poolReturn(conn), silent = TRUE))

  # create table with primary keys in i and j

  if(dbms(p) %in% c('duckdb', 'oracle')) {
    sql = dbplyr::build_sql(
      con = conn,
      'CREATE TABLE ', fnq, ' (',
      'i VARCHAR,',
      'j VARCHAR,',
      'x DOUBLE,', # will not be computing non numeric matrices
      'PRIMARY KEY (i, j)',
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





appendPermanent = function() {

}





# Table creation
# @name create_table_sql
# @title Create the SQL needed to create a new empty table
# @description
#
# @param conn DBI connection, pool, or hashID
# @param remote_name name to assign new table in the database
# @param pk primary key to set
# @param overwrite whether to overwrite if remote_name already exists in DB
# @param ... additional params to pass
# @keywords internal
# create_table_sql = function(conn,
#                             remote_name,
#                             pk,
#                             overwrite = FALSE,
#                             ...) {
#   p = evaluate_conn(conn, mode = 'pool')
#   fullNameQuoted = quote_full_table_name_quoted(
#     conn = conn,
#     remote_name = remote_name
#   )
#   if(existsTableBE(x = p, remote_name = remote_name)) {
#     if(isTRUE(overwrite)) {
#       DBI::dbRemoveTable(p, DBI::SQL(fullNameQuoted))
#     }
#     else {
#       stopf(fullNameQuoted, 'already exists.
#           Set overwrite = TRUE to recreate it.')
#     }
#   }
#
# }




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








#' @name get_full_table_name_quoted
#' @title Get the full table name consisting of the schema and table name
#' @description
#' Get the full name of a table. This information is then quoted to protect against
#' SQL injections.
#' Either param x or hash must be given
#' @param conn a hashID, DBI connection object, or pool object
#' @param remote_name name of table within DB
#' @return return the full table name
#' @keywords internal
get_full_table_name_quoted = function(conn, remote_name) {
  p = evaluate_conn(conn, mode = 'pool')

  stopifnot(is.character(remote_name))
  stopifnot(length(remote_name) == 1L)

  full_name_quoted = DBI::dbQuoteIdentifier(p, remote_name)

  return(full_name_quoted)
}





#' @name tableInfo
#' @title Get information about the table
#' @param conn hashID of backend, DBI connection or pool
#' @param remote_name name of table on DB
#' @return a data.table of information about the existing DB tables
tableInfo = function(conn, remote_name) {
  conn = evaluate_conn(conn, mode = 'conn')
  on.exit(pool::poolReturn(conn))
  sql_statement = dbplyr::build_sql(
    con = conn,
    'PRAGMA table_info(', remote_name,')'
  )
  res = DBI::dbGetQuery(conn = conn, statement = sql_statement)
  res = data.table::setDT(res)

  return(res)
}



#' @name primaryKey
#' @title Show if table has any primary keys
#' @param conn hashID of backend, DBI connection, or pool
#' @param remote_name name of table on DB
primaryKey = function(conn, remote_name) {
  name = pk = NULL
  res = tableInfo(conn, remote_name)[pk == TRUE, name]
  if(length(res) == 0L) return(NULL)
  res
}






#' @name dropTableBE
#' @title Drop a table from the database
#' @param conn connection object or pool
#' @param remote_name name of table to drop
#' @export
dropTableBE = function(conn, remote_name) {
  con = evaluate_conn(conn, mode = 'conn')
  on.exit(pool::poolReturn(con))
  fnq = get_full_table_name_quoted(conn = con, remote_name = remote_name)
  sql = dbplyr::build_sql('DROP TABLE ', fnq, con = con)
  DBI::dbExecute(con, sql)
  return(invisible())
}







# as.matrix ####
setMethod('as.matrix', signature(x = 'dbMatrix'), function(x, ...) {
  message('Pulling DB matrix into memory...')
  p_tbl = x@data %>%
    tidyr::pivot_wider(names_from = 'j', values_from = 'x') %>%
    dplyr::collect()

  mtx = p_tbl %>%
    dplyr::select(-i) %>%
    as('matrix')

  rownames(mtx) = p_tbl$i

  return(mtx)
})



