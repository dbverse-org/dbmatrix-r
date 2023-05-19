

# collate #
#' @include generics.R
NULL


# initialize backendInfo ####

#' @keywords internal
#' @noRd
setMethod('initialize', signature(.Object = 'backendInfo'),
          function(.Object, driver_call, db_path, ...) {

            if(!missing(driver_call)) .Object@driver_call = driver_call
            if(!missing(db_path)) .Object@db_path = db_path

            if(!is.na(.Object@db_path)) {
              .Object@hash = calculate_backend_id(.Object@db_path)
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
                p = getBackendPool(backend_ID = .Object@hash)
                tBE = tableBE(cPool = p, remote_name = remote_name)
                .Object@data = tBE
              }
            }

            if(is.null(.Object@data)) {
              stopf('data slot must contain a dplyr lazy table connected to the database backend')
            }
            if(!inherits(.Object@data, 'tbl_Pool')) {
              stopf('data slot only accepts dplyr class "tbl_Pool"')
            }

            validObject(.Object)
            return(.Object)
          })






# Determine if table already exists. If it does then send an error if overwrite
# FALSE or remove the table if overwrite is TRUE in preparation for recreation
overwrite_handler = function(p, remote_name, overwrite = FALSE) {
  stopifnot(is.character(remote_name) & length(remote_name) == 1L)
  p = evaluate_conn(p, mode = 'pool')

  # overwrite
  if(existsTableBE(p, remote_name = remote_name)) {
    if(isTRUE(overwrite)) {
      DBI::dbRemoveTable(p, remote_name)
    }
    else {
      stopf(remote_name, 'already exists.
          Set overwrite = TRUE to recreate it.')
    }
  }
}





# Create a sequence in the database. Useful for creating auto-incrementing columns
# This value is applied during row insertion
# create_sequence = function(conn, name = 'my_seq', start = 1L, increment = 1L, overwrite = FALSE) {
#   stopifnot(length(start) == 1L & is.numeric(start))
#   stopifnot(length(increment) == 1L & is.numeric(increment))
#   stopifnot(length(name) == 1L & is.character(name))
#
#   conn = evaluate_conn(conn)
#   q_name = DBI::dbQuoteIdentifier(conn, name)
#   statement =
#     paste('CREATE SEQUENCE', q_name, 'START WITH', start, 'INCREMENT BY', increment)
#   # overwrite if necessary
#   try_val = try(invisible(DBI::dbExecute(conn, statement)), silent = TRUE)
#   if(inherits(try_val, 'try-error')) {
#     if(overwrite) {
#       DBI::dbExecute(conn, paste('DROP SEQUENCE', q_name))
#       invisible(DBI::dbExecute(conn, statement))
#     } else {
#       stopf('sequence to create already exists. Set overwrite = TRUE to replace')
#     }
#   }
# }











# compute ####

#' @noRd
compute_temporary = function(x, p, fnq, ...) {
  x[] = x[] %>%
    dbplyr::compute(name = as.character(fnq),
                    temporary = TRUE,
                    ...)
  x@remote_name = as.character(fnq)

  return(x)
}





# create_index ####

#' @name create_index
#' @title Create an index on specified columns
#' @param x dbData object
#' @param name character. Name of index to create
#' @param column character. Name(s) of columns to set as the index
#' @param unique logical. Default = FALSE, Whether unique constraint can be applied
#' to specified column
#' @param ... additional params to pass
#' @keywords internal
#' @noRd
setMethod('create_index', signature(x = 'dbData', name = 'character',
                                    column = 'character', unique = 'logical'),
          function(x, name, column, unique = FALSE, ...) {
            x = reconnect(x)
            p = cPool(x)
            q = lapply(list(name = name, remote = remoteName(x), col = column),
                       function(ids) DBI::dbQuoteIdentifier(p, ids))
            statement = paste0('CREATE INDEX ', q$name, ' ON ', q$remote,
                               ' (', paste(q$col, collapse = ', '), ')')
            if(isTRUE(unique)) statement =
              gsub('CREATE INDEX', 'CREATE UNIQUE INDEX', statement)
            DBI::dbExecute(p, statement = statement)
          })





# appendPermanent = function(x, remote_name, ...) {
#   bID = backendID(x)
#   p = getBackendPool(backend_ID = bID)
#   fnq = get_full_table_name_quoted(p, remote_name)
#   # if table to append to does not exist, create it
#   if(!existsTableBE(x = p, remote_name = remote_name)) {
#     return(compute_permanent(x = x, p = p, fnq = fnq, ...))
#   }
# }





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
  res = tableInfo(conn, remote_name)
  res = res[pk == TRUE, name]
  if(length(res) == 0L) return(NULL)
  res
}






# data removal ####




#' @name dropTableBE
#' @title Drop a table from the database
#' @param conn connection object or pool
#' @param remote_name name of table to drop
#' @export
dropTableBE = function(conn, remote_name) {
  con = evaluate_conn(conn, mode = 'conn')
  on.exit(pool::poolReturn(con))
  fnq = get_full_table_name_quoted(conn = conn, remote_name = remote_name)
  sql = dbplyr::build_sql('DROP TABLE ', fnq, con = con)
  DBI::dbExecute(con, sql)
  return(invisible())
}




# TODO implement row deletion
# @name deleteTableRow
# @title Delete row from table
# @param x dbData object
# @param y data.table of values (by column) to remove from x by matching
# @param verbose be verbose
# deleteTableRow = function(x, y, verbose = TRUE) {
#   assert_dbData(x)
#   assert_DT(y)
#
#   p = cPool(x)
#   quoted_name = DBI::dbQuoteIdentifier(remoteName(x))
#
# }

# DBI::dbExecute(p, paste0('DELETE FROM ', 'cell_rna_raw', ' WHERE (j = \'AATK\' AND i = \'ID_1_0\')'))







