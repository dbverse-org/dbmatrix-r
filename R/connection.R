


# driver ####

#' @name driver-generic
#' @title Database driver generation
#' @description
#' Set or get the driver/database instance generation function call
#' @aliases driver, driver<-
#' @export
setMethod('driver', signature(x = 'dbData'), function(x) x@dvr_call)

#' @rdname driver-generic
#' @export
setMethod('driver<-', signature(x = 'dbData'), function(x, value) {
  x@dvr_call = substitute(value)
  x
})





# connection ####

#' @name connection-generic
#' @title Database connection
#' @description Return the database connection object
#' @aliases connection, connection<-
#' @inheritParams db_params
#' @include extract.R
#' @export
setMethod('connection', signature(x = 'dbData'), function(x) {
  dbplyr::remote_con(x[])
})

#' @rdname connection-generic
#' @export
setMethod('connection<-', signature(x = 'dbData'), function(x, value) {
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







# remoteValid ####
#' @name remoteValid
#' @title Check if DB backend object has valid connection
#' @inheritParams db_params
#' @export
setMethod('remoteValid', signature(x = 'dbData'), function(x) {
  con = connection(x)
  if(is.null(con)) stop('No connection object found\n')
  return(DBI::dbIsValid(con))
})






# remoteListTables ####
#' @name remoteListTables
#' @title List the tables in a connection
#' @param x GiottoDB object or DB connection
#' @param ... additional params to pass
#' @export
setMethod('remoteListTables', signature(x = 'dbData'), function(x, ...) {
  stopifnot(remoteValid(x))
  DBI::dbListTables(connection(x), ...)
})

#' @rdname remoteListTables
#' @export
setMethod('remoteListTables', signature(x = 'ANY'), function(x, ...) {
  stopifnot(DBI::dbIsValid(x))
  DBI::dbListTables(x, ...)
})






# remoteExistsTable ####
#' @name remoteExistsTable
#' @title Whether a particular table exists in a connection
#' @param x GiottoDB object or DB connection
#' @param ... additional params to pass
#' @export
setMethod('remoteExistsTable',
          signature(x = 'dbData', remote_name = 'character'),
          function(x, remote_name, ...) {
            stopifnot(remoteValid(x))
            extabs = DBI::dbListTables(conn = connection(x), ...)
            remote_name %in% extabs
          })

#' @rdname remoteExistsTable
#' @export
setMethod('remoteExistsTable',
          signature(x = 'ANY', remote_name = 'character'),
          function(x, remote_name, ...) {
            stopifnot(DBI::dbIsValid(x))
            extabs = DBI::dbListTables(conn = x, ...)
            remote_name %in% extabs
          })







# reconnection ####

#' @name reconnect
#' @title Reconnect to database
#' @description
#' Reconnects to a database. Additionally checks that the object contains a
#' reference to a valid table within the database. Runs at the beginning of
#' most function calls
#' @inheritParams db_params
#' @param connect_type (either 'default' or 'custom') type of connection process
#' (optional because this value is usually carried by the object)
#' @param custom_call specific custom call to use when rebooting (optional)
#' @include query.R
#' @export
setMethod('reconnect', signature(x = 'dbData'),
          function(x, connect_type = NULL, custom_call) {

  if(!x@connect_type %in% c('default', 'custom')) {
    stop('Invalid connect_type. Must be one of "default" or "custom"\n')
  }

  # if connection is still active, return directly
  if(remoteValid(x)) return(x)



  # otherwise regenerate connection
  if(x@connect_type == 'default') {
    con = DBI::dbConnect(drv = eval(x@dvr_call), dbdir = x@path)
    connection(x) = con
    if(!x@remote_name %in% remoteListTables(x)) {
      stop(paste('GiottoDB-reconnect: Table of name', x@remote_name,
                 'not found in db connection.\n',
                 'Memory-only table that was never written to DB?'),
           call. = FALSE)
    }
  }

  if(x@connect_type == 'custom' | identical(connect_type, 'custom')) {
    qstack = queryStack(x)

    if(!missing(custom_call)) {
      ccsub = substitute(custom_call)
      new_tbl = eval(cc)
    } else {
      new_tbl = eval(x@custom_call)
    }

    queryStack(new_tbl) = qstack
    x@data = new_tbl
  }

  return(x)
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
  DBI::dbDisconnect(connection(x), shutdown = shutdown)
  return(x)
})








