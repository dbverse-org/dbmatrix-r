



# hash ####
#' @name hashBE-generic
#' @title Get hash of backend
#' @description
#' Get the hash value/slot
#' @param x object to get hash from
#' @export
setMethod('hashBE', signature(x = 'backendInfo'), function(x) {
  slot(x, 'hash')
})















# cPool ####

#' @name cPool-generic
#' @title Database connection pool
#' @description Return the database connection pool object
#' @aliases cPool, cPool<-
#' @inheritParams db_params
#' @include extract.R
#' @export
setMethod('cPool', signature(x = 'dbData'), function(x) {
  dbplyr::remote_con(x[])
})

#' @rdname cPool-generic
#' @export
connection.tbl_Pool = function(x) {
  dbplyr::remote_con(x)
}

#' @rdname cPool-generic
#' @export
setMethod('cPool<-', signature(x = 'dbData'), function(x, value) {
  dbtbl = x[]
  dbtbl$src$con = value
  initialize(x, data = dbtbl)
})



# remoteName ####
#' @name remoteName-generic
#' @title Database table name
#' @description
#' Returns the table name within the database the the object acts as a link to
#' @aliases remoteName
#' @inheritParams db_params
#' @export
setMethod('remoteName', signature(x = 'dbData'), function(x) {
  slot(x, 'data') %>%
    dbplyr::remote_name() %>%
    as.character()
})







# validBE ####
#' @name validBE-generic
#' @title Check if DB backend object has valid connection
#' @aliases validBE
#' @inheritParams db_params
#' @export
setMethod('validBE', signature(x = 'dbData'), function(x) {
  con = cPool(x)
  if(is.null(con)) stop('No connection object found\n')
  return(DBI::dbIsValid(con))
})






# listTablesBE ####
#' @name listTablesBE
#' @title List the tables in a connection
#' @param x GiottoDB object or DB connection
#' @param ... additional params to pass
#' @export
setMethod('listTablesBE', signature(x = 'dbData'), function(x, ...) {
  stopifnot(remoteValid(x))
  DBI::dbListTables(connection(x), ...)
})

#' @rdname listTablesBE
#' @export
setMethod('listTablesBE', signature(x = 'ANY'), function(x, ...) {
  stopifnot(DBI::dbIsValid(x))
  DBI::dbListTables(x, ...)
})






# existsTableBE ####
#' @name existsTableBE
#' @title Whether a particular table exists in a connection
#' @param x GiottoDB object or DB connection
#' @param ... additional params to pass
#' @export
setMethod('existsTableBE',
          signature(x = 'dbData', remote_name = 'character'),
          function(x, remote_name, ...) {
            stopifnot(remoteValid(x))
            extabs = DBI::dbListTables(conn = cPool(x), ...)
            remote_name %in% extabs
          })

#' @rdname existsTableBE
#' @export
setMethod('existsTableBE',
          signature(x = 'ANY', remote_name = 'character'),
          function(x, remote_name, ...) {
            stopifnot(DBI::dbIsValid(x))
            extabs = DBI::dbListTables(conn = x, ...)
            remote_name %in% extabs
          })











# disconnection ####

#' @name disconnect
#' @title Disconnect from database
#' @description
#' Disconnects from a database. Closes both the connection and driver objects
#' by default
#' @inheritParams db_params
#' @export
setMethod('disconnect', signature(x = 'dbData'), function(x, shutdown = TRUE) {
  # already closed
  if(!remoteValid(x)) return(x)
  pool::poolClose(cPool(x), force = TRUE)
  return(x)
})






#' @title Create a pool of database connections
#' @name create_connection_pool
#' @description Generate a pool object from which connection objects can be
#' checked out.
#' @param drv DB driver (default is duckdb::duckdb())
#' @param dbpath path to database
#' @param ... additional params to pass to pool::dbPool()
#' @return pool of connection objects
#' @keywords internal
create_connection_pool = function(drv = 'duckdb::duckdb()',
                                  dbdir = ':memory:',
                                  ...) {
  pool::dbPool(drv = eval(parse(text = drv)), dbdir = dbdir, ...)
}





#' @title Create GiottoDB backend
#' @name createBackend
#' @description
#' Defines and creates a database connection pool to be used by the backend.
#' This pool object and a backendInfo object that contains the connection
#' details for reconnection
#'
#' @param drv database driver (default is duckdb::duckdb())
#' @param dbdir directory to create a backend database
#' @param extension file extension (default = '.duckdb')
#' @param ... additional params to pass to pool::dbPool()
#' @return environment containing backendInfo object
#' @export
createBackend = function(drv = duckdb::duckdb(),
                        dbdir = ':temp:',
                        extension = '.duckdb',
                        ...) {

  additional_params = list(...)

  # store driver call as a string
  drv_call = deparse(substitute(drv))

  # setup database filepath
  dbpath = set_db_path(path = dbdir, extension = extension)

  # collect connection information
  gdb_info = new('backendInfo',
                 driver_call = drv_call,
                 db_path = dbpath,
                 add_params = additional_params)

  # assemble params
  list_args = additional_params
  list_args$drv = drv_call
  list_args$dbdir = dbpath

  # get connection pool and store info
  con_pool = do.call(what = 'create_connection_pool', args = list_args)

  backend_list = list(info = gdb_info,
                      pool = con_pool)

  .DB_ENV[[hashBE(gdb_info)]] = backend_list
  invisible()
}





#' @title Get the backend details environment
#' @name getBackendEnv
#' @export
getBackendEnv = function() {
  return(.DB_ENV)
}





#' @name getBackendPool
#' @description
#' Get backend information. \code{getBackendPool} gets the associated
#' connection pool object. \code{getBackendInfo} gets the backendInfo object
#' that contains all connection details needed to regenerate another connection
#' @param hash hash generated from the dbpath (xxhash64) used as unique ID for
#' the backend
#' @export
getBackendPool = function(hash) {
  .DB_ENV[[hash]]$pool
}

#' @describeIn getBackendInfo get backendInfo object containing connection details
#' @export
getBackendInfo = function(hash) {
  .DB_ENV[[hash]]$info
}





# reconnectBackend ####

#' @title Reconnect GiottoDB backend
#' @name reconnectBackend-generic
#' @aliases reconnectBackend
#' @param x backendInfo object
#' @export
setMethod('reconnectBackend', signature('backendInfo'), function(x, ...) {

  hash = hashBE(x)

  # if already valid, exit
  if(DBI::dbIsValid(.DB_ENV[[hash]]$pool)) return(invisible())


  # assemble params
  list_args = x@add_params
  list_args$drv = x@driver_call
  list_args$dbdir = x@db_path

  # regenerate connection pool object
  con_pool = do.call(what = 'create_connection_pool', args = list_args)
  .DB_ENV[[hash]]$pool = con_pool
  .DB_ENV[[hash]]$info = x

  return(invisible())
})








#' @name closeBackend
#' @title Close backend connection pool
#' @description
#' Closes pools. If specific hash(es) are given then those will be closed. When
#' no specific hash is given, all existing backends will be closed.
#' @param hash hash of backend to close (optional)
#' @export
closeBackend = function(hash) {

  if(missing(hash)) {
    hash = ls(.DB_ENV)
  }

  lapply(hash, function(x) {
    pool::poolClose(.DB_ENV[[x]]$pool)
  })

  return(invisible())
}




# dbData reconnection ####

#' @name reconnect
#' @title Reconnect to backend
#' @description
#' Checks if the object still has a valid connection. If it does then it returns
#' the object without modification. If it does need reconnection then a copy of
#' the associated active connection pool object is pulled from .DB_ENV and if
#' that is also closed, an error is thrown asking for a reconnectBackend() run.
#'
#' Also ensures that the object contains a reference to a valid table within
#' the database. Runs at the beginning of most function calls involving
#' dbData object queries.
#' @inheritParams db_params
#' @include query.R
#' @export
setMethod('reconnect', signature(x = 'dbData'),
          function(x) {

            # if connection is still active, return directly
            if(validBE(x)) return(x)


            # otherwise get conn pool
            p = .DB_ENV[[x@hash]]$pool
            if(!validBE(p)) {
              stopf('Backend needs to be created or reconnected.
                    Run reconnectBackend()?')
            }

            cPool(x) = p

            if(!x@remote_name %in% listTablesBE(x)) {
              stopf('Reconnection attempted, but table of name', x@remote_name,
                    'not found in db connection.\n',
                    'Memory-only table that was never written to DB?')
            }

            return(x)
          })









# DB table management ####



#' @title Write a persistent table to the database
#' @name writeRemoteTable
#' @param cPool database connection pool object
#' @param remote_name name to assign the table within the database
#' @param value table to write
#' @param ... additional params to pass to DBI::dbWriteTable()
#' @export
writeTableBE = function(cPool, remote_name, value, ...) {
  DBI::dbWriteTable(conn = cPool, name = remote_name, value = value, ...)
}



#' @title Create a table from database
#' @name tableBE
#' @param cPool database connection pool object
#' @param remote_name name of table with database
#' @param ... additional params to pass to dplyr::tbl()
#' @export
tableBE = function(cPool, remote_name, ...) {
  dplyr::tbl(src = cPool, remote_name, ...)
}







