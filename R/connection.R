



# backendID ####
#' @name backendID-generic
#' @title Get and set hash ID of backend
#' @description
#' Get the hash value/slot
#' @aliases backendID, backendID<-
#' @param x object to get hash from
#' @param value (character) hash ID to set
#' @export
setMethod('backendID', signature(x = 'backendInfo'), function(x) {
  slot(x, 'hash')
})
#' @rdname backendID-generic
#' @export
setMethod('backendID', signature(x = 'dbData'), function(x) {
  slot(x, 'hash')
})


#' @rdname backendID-generic
#' @export
setMethod('backendID<-', signature(x = 'backendInfo', value = 'character'),
          function(x, ..., value) {
            slot(x, 'hash') = value
          })
#' @rdname backendID-generic
#' @export
setMethod('backendID<-', signature(x = 'dbData', value = 'character'),
          function(x, ..., value) {
            slot(x, 'hash') = value
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
setMethod('cPool<-', signature(x = 'dbData'), function(x, value) {
  dbtbl = x[]
  dbtbl$src$con = value
  initialize(x, data = dbtbl)
})

#' @rdname cPool-generic
#' @export
setMethod('cPool', signature(x = 'ANY'), function(x) {
  stopifnot(inherits(x, 'tbl_sql'))
  dbplyr::remote_con(x)
})
#' @rdname cPool-generic
#' @export
setMethod('cPool<-', signature(x = 'ANY'), function(x, value) {
  stopifnot(inherits(x, 'tbl_sql'))
  x$src$con = value
  x
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
  slot(x, 'remote_name')
})




# is_init ####
#' @name is_init-generic
#' @title Determine if dbData object is initialized
#' @param x dbData object
#' @keywords internal
setMethod('is_init', signature(x = 'dbData'), function(x, ...) {
  slot(x, 'init')
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
  DBI::dbListTables(cPool(x), ...)
})

#' @rdname listTablesBE
#' @export
setMethod('listTablesBE', signature(x = 'character'), function(x, ...) {
  x_pool = try(getBackendPool(x), silent = TRUE) # should be backend_ID
  if(inherits(x_pool, 'try-error')) {
    x_pool = getBackendID(x)
  }
  stopifnot(DBI::dbIsValid(x_pool))
  DBI::dbListTables(x_pool, ...)
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
#' @param with_login (default = FALSE) flag to check R environment variables
#' for login info and/or prompt for password
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
#' This pool object and a backendInfo object that contains details about the
#'
#' @param drv database driver (default is duckdb::duckdb())
#' @param dbdir directory to create a backend database
#' @param extension file extension (default = '.duckdb')
#' @param with_login (default = FALSE) whether a login is needed
#' @param verbose be verbose
#' @param ... additional params to pass to pool::dbPool()
#' @return invisibly returns backend ID
#' @export
createBackend = function(drv = duckdb::duckdb(),
                         dbdir = ':temp:',
                         extension = '.duckdb',
                         with_login = FALSE,
                         verbose = TRUE,
                         ...) {

  # store driver call as a string
  drv_call = deparse(substitute(drv))

  # setup database filepath
  dbpath = set_db_path(path = dbdir, extension = extension, verbose = verbose)

  # collect connection information
  gdb_info = new('backendInfo',
                 driver_call = drv_call,
                 db_path = dbpath)
  if(isTRUE(verbose)) wrap_msg(
    ' Backend Identifier:', backendID(gdb_info)
  )

  # assemble params
  list_args = list(...)
  if(isTRUE(with_login)) { # check login details
    if(is.null(list_args$user)) list_args$user = Sys.getenv('DB_USERNAME')
    if(is.null(list_args$pass)) {
      if(requireNamespace('rstudioapi', quietly = TRUE)) {
        list_args$pass =
          Sys.getenv('DB_PASSWORD', rstudioapi::askForPassword('Database Password'))
      } else {
        list_args$pass = Sys.getenv('DB_PASSWORD')
      }
    }
  }
  list_args$drv = drv_call
  list_args$dbdir = dbpath

  # get connection pool and store info
  con_pool = do.call(what = 'create_connection_pool', args = list_args)

  backend_list = list(info = gdb_info,
                      pool = con_pool)

  .DB_ENV[[backendID(gdb_info)]] = backend_list
  invisible(backendID(gdb_info))
}





#' @title Get the backend details environment
#' @name getBackendEnv
#' @export
getBackendEnv = function() {
  return(.DB_ENV)
}





#' @name getBackendPool
#' @title Get backend connection pool
#' @description
#' Get backend information. \code{getBackendPool} gets the associated
#' connection pool object. \code{getBackendInfo} gets the backendInfo object
#' that contains all connection details needed to regenerate another connection
#' @param backend_ID hashID generated from the dbpath (xxhash64) used as unique ID for
#' the backend
#' @export
getBackendPool = function(backend_ID) {
  p = try(.DB_ENV[[backend_ID]]$pool, silent = TRUE)
  if(is.null(p) | inherits(p, 'try-error')) {
    stopf('No associated backend found.
          Please create or reconnect the backend.')
  }
  return(p)
}

#' @describeIn getBackendPool get backendInfo object containing connection details
#' @export
getBackendInfo = function(backend_ID) {
  i = try(.DB_ENV[[backend_ID]]$info, silent = TRUE)
  if(is.null(i) | inherits(p, 'try-error')) {
    stopf('No associated backend found.
          Please create or reconnect the backend.')
  }
  return(i)
}





#' @describeIn getBackendPool Get a DBI connection object from pool.
#' Provided for convenience. Must be returned using pool::poolReturn() after use.
#' @export
getBackendConn = function(backend_ID) {
  p = getBackendPool(backend_ID)
  pool::poolCheckout(p)
}




#' @describeIn getBackendPool get filepath of backend
#' @export
getBackendPath = function(backend_ID) {
  conn = evaluate_conn(conn = backend_ID, mode = 'conn')
  on.exit(pool::poolReturn(conn))
  conn@driver@dbdir
}





# evaluate_conn ####
# Internal function to allow functions to take connection inputs as a DBI
# connection object, Pool of connections, or a hash ID of a backend
# The connections outputted will be preferentially a pool, but if a DBI
# connection is given as input, it will also be returned. This default can also
# be overridden using the mode = 'pool' or mode = 'conn' param for hash or pool
# inputs
setMethod('evaluate_conn', signature(conn = 'character'),
          function(conn, mode = 'pool', ...)
{
  mode = match.arg(mode, choices = c('pool', 'conn'))
  if(mode == 'pool') {
    return(getBackendPool(conn))
  } else if(mode == 'conn') {
    return(getBackendConn(conn))
  }
})
setMethod('evaluate_conn', signature(conn = 'Pool'),
          function(conn, mode = 'pool', ...)
{
  mode = match.arg(mode, choices = c('pool', 'conn'))
  if(mode == 'pool') {
    return(conn)
  } else if(mode == 'conn') {
    return(pool::poolCheckout(conn))
  }
})
setMethod('evaluate_conn', signature(conn = 'DBIConnection'),
          function(conn, mode = 'conn', ...)
{
  if(mode == 'pool') stopf('Cannot get pool from conn object')
  if(class(conn) %in% c("Microsoft SQL Server",
                        "PqConnection",
                        "RedshiftConnection",
                        "BigQueryConnection",
                        "SQLiteConnection",
                        "duckdb_connection",
                        "Spark SQL",
                        "OraConnection",
                        "Oracle",
                        "Snowflake")) {
    return(conn)
  } else {
    stopf(class(conn), "is not a supported connection type.")
  }
})






# reconnectBackend ####

#' @title Reconnect GiottoDB backend
#' @name reconnectBackend-generic
#' @aliases reconnectBackend
#' @param x backendInfo object
#' @param with_login (default = FALSE) flag to check R environment variables
#' @param verbose be verbose
#' @param ... additional params to pass
#' for login info and/or prompt for password
#' @export
setMethod(
  'reconnectBackend', signature('backendInfo'),
  function(x, with_login = FALSE, verbose = TRUE, ...)
{
  b_ID = backendID(x)
  # if already valid, exit
  try_val = try(DBI::dbIsValid(.DB_ENV[[b_ID]]$pool), silent = TRUE)
  if(isTRUE(try_val)) return(invisible())


  # assemble params
  list_args = list(...)
  if(isTRUE(with_login)) { # check login details
    if(is.null(list_args$user)) list_args$user = Sys.getenv('DB_USERNAME')
    if(is.null(list_args$pass)) {
      if(requireNamespace('rstudioapi', quietly = TRUE)) {
        list_args$pass =
          Sys.getenv('DB_PASSWORD', rstudioapi::askForPassword('Database Password'))
      } else {
        list_args$pass = Sys.getenv('DB_PASSWORD')
      }
    }
  }
  list_args$drv = x@driver_call
  list_args$dbdir = x@db_path

  # regenerate connection pool object
  con_pool = do.call(what = create_connection_pool, args = list_args)
  .DB_ENV[[b_ID]]$pool = con_pool
  .DB_ENV[[b_ID]]$info = x

  return(invisible())
})








#' @name closeBackend
#' @title Close backend connection pool
#' @description
#' Closes pools. If specific backend_ID(s) are given then those will be closed.
#' When no specific ID is given, all existing backends will be closed.
#' @param backend_ID hashID of backend to close (optional)
#' @export
closeBackend = function(backend_ID) {

  if(missing(backend_ID)) {
    backend_ID = ls(.DB_ENV)
  }

  lapply(backend_ID, function(x) {
    conn = getBackendConn(x)
    DBI::dbDisconnect(conn, shutdown = TRUE)
    pool::poolReturn(conn)
    suppressWarnings(pool::poolClose(getBackendPool(x)))
  })

  return(invisible())
}




#' @name existingHashIDs
#' @title Get all existing backend hash IDs
#' @export
existingHashIDs = function() {
  ls(.DB_ENV)
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





# DBMS detection ####

# From package CDMConnection

#' @name dbms-generic
#' @title Database management system
#' @aliases dbms
#' @description
#' Get the database management system (dbms) of an object
#' @param x A connection pool object, DBI connection, or dbData
#' @param ... additional params to pass
#' @export
setMethod('dbms', signature(x = 'dbData'), function(x, ...) {
  dbms(cPool(x))
})
#' @rdname dbms-generic
#' @export
setMethod('dbms', signature(x = 'character'), function(x, ...) {
  dbms(evaluate_conn(x, mode = 'pool'))
})

#' @rdname dbms-generic
#' @importClassesFrom pool Pool
#' @export
setMethod('dbms', signature(x = 'Pool'), function(x, ...) {
  conn = pool::poolCheckout(x)
  on.exit(pool::poolReturn(conn))
  res = dbms(conn)
  return(res)
})
#' @rdname dbms-generic
#' @export
setMethod('dbms', signature(x = 'DBIConnection'), function(x, ...) {
  if (!is.null(attr(x, "dbms")))
    return(attr(x, "dbms"))
  result = switch(
    class(x),
    "Microsoft SQL Server" = "sql server",
    "PqConnection" = "postgresql",
    "RedshiftConnection" = "redshift",
    "BigQueryConnection" = "bigquery",
    "SQLiteConnection" = "sqlite",
    "duckdb_connection" = "duckdb",
    "Spark SQL" = "spark",
    "OraConnection" = "oracle",
    "Oracle" = "oracle",
    "Snowflake" = "snowflake"
    # add mappings from various connection classes to dbms here
  )
  if (is.null(result)) {
    stopf(class(x), "is not a supported connection type.")
  }
  return(result)
})









