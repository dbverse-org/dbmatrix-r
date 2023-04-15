

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
                          ...) {

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

  db_path = set_db_path(db_path)
  hash = calculate_hash(db_path)

  p = getBackendPool(hash)
  conn = pool::poolCheckout(p)
  DBI::dbWriteTable(conn = conn,
                    name = remote_name,
                    value = ijx,
                    ...)
  pool::poolReturn(conn)


  dbMat = new('dbMatrix',
              hash = hash,
              remote_name = remote_name,
              dim_names = list(rownames(matrix),
                               colnames(matrix)),
              dims = dim(matrix))

  return(dbMat)
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



