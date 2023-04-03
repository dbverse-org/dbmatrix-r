


# driver ####

#' @name driver-generic
#' @title Database driver generation
#' @description
#' Set or get the driver/database instance generation function
#' @aliases driver, driver<-
#' @include generics.R
#' @export
setMethod('driver', signature(x = 'dbData'), function(x) x@driver)

#' @rdname driver-generic
#' @export
setMethod('driver<-', signature(x = 'dbData'), function(x, value) {
  x@driver = value
  x
})





# connection ####

#' @name connection-generic
#' @title Database connection
#' @description Return the database connection object
#' @aliases connection, connection<-
#' @inheritParams db_params
#' @include generics.R
#' @export
setMethod('connection', signature(x = 'dbData'), function(x) x@connection)

#' @rdname connection-generic
#' @export
setMethod('connection<-', signature(x = 'dbData'), function(x, value) {
  initialize(x, connection = value)
})




#' @name tblName-generic
#' @title Database table name
#' @description
#' Returns the table name within the database the the object acts as a link to
#' @aliases tblName, tblName<-
#' @inheritParams db_params
#' @include generics.R
#' @export
setMethod('tblName', signature(x = 'dbData'), function(x) x@table_name)

#' @rdname tblName-generic
#' @export
setMethod('tblName<-', signature(x = 'dbData'), function(x, value) {
  initialize(x, table_name = value)
})



# reconnection ####

#' @name reconnect
#' @title Reconnect to database
#' @description
#' Reconnects to a database. Additionally checks that the object contains a
#' reference to a valid table within the database. Runs at the beginning of
#' most function calls
#' @inheritParams db_params
#' @include generics.R
#' @export
setMethod('reconnect', signature(x = 'dbData'), function(x) {

  if(!isTRUE(x@valid) | is.null(driver(x))) {
    stop('Invalid connection. A connection driver and table name are required\n
         Provide during construction or use driver<-() / tblName()<-\n')
  }

  # if connection is still active, return directly
  if(DBI::dbIsValid(connection(x))) return(x)

  # otherwise regenerate connection
  con = DBI::dbConnect(drv = driver(x)(), x@path)
  connection(x) = con

  return(x)
})




# disconnection ####

#' @name disconnect
#' @title Disconnect from database
#' @description
#' Disconnects from a database. Closes both the connection and driver objects
#' by default
#' @inheritParams db_params
#' @include generics.R
#' @export
setMethod('disconnect', signature(x = 'dbData'), function(x, shutdown = TRUE) {

  con = connection(x)
  if(is.null(con)) stop('No connection object found\n')

  # already closed
  if(!DBI::dbIsValid(con)) return(x)

  DBI::dbDisconnect(con, shutdown = shutdown)

  return(x)
})








