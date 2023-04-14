



#' @keywords internal
#' @noRd
setMethod('initialize', signature(.Object = 'backendInfo'),
          function(.Object, driver_call, db_path, add_params, ...) {

            if(!missing(driver_call)) .Object@driver_call = driver_call
            if(!missing(db_path)) .Object@db_path = db_path
            if(!missing(add_params)) .Object@add_params = add_params

            if(!is.na(.Object@db_path)) {
              .Object@hash = calculate_hash(.Object@db_path)
            }

            validObject(.Object)
            return(.Object)

          })




# initialize dbData ####
# setMethod('initialize', signature(.Object = 'dbData'),
#           function(.Object,
#                    dvr,))






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
  function(.Object,
           dvr_call,
           custom_call,
           data,
           path,
           remote_name,
           connect_type,
           dim_names,
           dim,
           ...) {

    # data input #
    # ---------- #

    if(!missing(dvr_call)) .Object@dvr_call = dvr_call
    if(!missing(custom_call)) .Object@custom_call = custom_call
    if(!missing(remote_name)) .Object@remote_name = remote_name
    if(!missing(connect_type)) .Object@connect_type = connect_type

    if(!missing(data)) {
      if(!inherits(data, 'tbl_dbi')) {
        stopf('Data slot only accepts dplyr::tbl() connections to databases')
      }
      .Object@data = data

      con_path = DBI::dbGetInfo(dbplyr::remote_con(data))$dbname
      if(con_path != ':memory:') con_path = normalizePath(con_path)
      .Object@path = con_path
    }


    if(!missing(path)) {
      if(!is.null(.Object$path)) {
        wrap_msg('Replacing path to DB...
                 NOTE: It is preferred to omit path param in favor of',
                 'detection from data input')
      }
      .Object$path = path
    }

    # matrix specific
    if(!missing(dim_names)) .Object@dim_names = dim_names
    if(!missing(dim)) .Object@dim = dim





    # generate default DB connection if no input provided #
    # --------------------------------------------------- #
    if(is.null(.Object@data)) {
      .Object@path = if(is.na(.Object@path)) ':memory:'
      .Object@dvr_call = if(is.null(.Object@dvr_call)) substitute(duckdb::duckdb())
      .Object@remote_name = if(is.na(.Object@remote_name)) 'test'
      .Object@connect_type = 'default'

      test_table = data.table::data.table(i = 'a', j = 'b', x = NA_integer_) # TODO update if matrix uses integer indexing

      con = DBI::dbConnect(drv = eval(.Object@dvr_call), dbdir = .Object@path)
      DBI::dbCreateTable(conn = con,
                         name = .Object@remote_name,
                         fields = test_table)
      .Object@data = dplyr::tbl(con, .Object@remote_name)

      .Object@dim = c(1L, 1L)
      .Object@dim_names = list('a', 'b')
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
#' @param db_name name to assign within database
#' @param db_connection a db connection object
#' @param db_path path to database on disk
#' @param db_driver db driver generation function. Default is duckdb::duckdb()
#' @param to_triplet (default = TRUE) conversion to triplet (ijx) is needed
#' @param custom_call non-default calls for generating a database connection
#' @param dim_names list of rownames and colnames of the matrix (optional)
#' @param dim dimensions of the matrix (optional)
#' @param cores number of cores to use if reading into R
#' @include matrix_to_dt.R
#' @noRd
createDBMatrix = function(matrix,
                          remote_name = 'test',
                          db_connection,
                          db_path = ':memory:',
                          db_driver,
                          to_triplet = TRUE,
                          custom_call,
                          dim_names,
                          dim,
                          cores = 1L) {

  # store driver call
  if(!missing(db_driver)) {
    drv_sub = substitute(db_driver)
  } else {
    drv_sub = substitute(duckdb::duckdb())
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
    ijx = get_Ijx_Zero_DT(matrix)
  }


  if(is.character(db_path)) {

    if(db_path != ':memory:') {
      # Generate path to DB save location
      db_path = path.expand(db_path)
      if(!file.exists(db_path)) {
        dbdir = gsub(basename(db_path), '/', db_path)
        if(!dir.exists(dbdir)) dir.create(dbdir, recursive = TRUE)
      }
    }

    con = DBI::dbConnect(drv = eval(drv_sub), dbdir = db_path)

    DBI::dbWriteTable(con, db_name, ijx)
    db_tbl = dplyr::tbl(con, db_name)

  }




  # TODO edit dim_names and dim input to allow for workflows that do not read into mem
  dbMat = new('dbMatrix',
              dvr_call = drv_sub,
              data = db_tbl,
              remote_name = db_name,
              dim_names = list(rownames(matrix),
                               colnames(matrix)),
              dim = dim(matrix))

}





# TODO create from tbl_dbi?

create_dbmatrix_matrix = function(matrix,
                                  db_name = 'test',
                                  db_path = ':memory:',
                                  db_driver = duckdb::duckdb(),
                                  to_triplet = TRUE,
                                  cores = 1L) {

}




# add key - no support ####
# Functions from https://github.com/schardtbc/DBIExt
# setMethod('setRemoteKey',
#           signature(x = 'dbData',
#                     remote_name = 'character',
#                     primary_key = 'character'),
#           function(x, remote_name, primary_key, ...) {
#             stopifnot(remoteValid(x),
#                       remoteExistsTable(x, remote_name))
#             cols_in_table = DBI::dbListFields(conn = connection(x),
#                                               name = remote_name)
#             stopifnot(setequal(primary_key, intersect(primary_key, cols_in_table)))
#             query = sql_set_remote_key(conn = connection(x),
#                                        remote_name = remote_name,
#                                        primary_key = primary_key)
#             DBI::dbExecute(conn = connection(x),
#                            statement = query)
#           })
#
#
#
#
#
# sql_set_remote_key = function(conn, remote_name, primary_key) {
#   table_q = DBI::dbQuoteIdentifier(conn, remote_name)
#   key_q = sapply(primary_key, function(x) {
#     DBI::dbQuoteIdentifier(conn, as.character(x))
#   })
#
#   sql_alter_table = DBI::SQL(paste0(
#     'ALTER TABLE ',
#     table_q,
#     '\n',
#     'ADD PRIMARY KEY (',
#     paste0(key_q, collapse = ', '),
#     ');'
#   ))
# }







toTable = function(query, remote_name) {
  dplyr::do(paste(unclass(query$query$sql), 'TO TABLE', remote_name))
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



