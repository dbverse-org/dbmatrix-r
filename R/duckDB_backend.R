






# initialize ####

setMethod(
  'initialize',
  signature(.Object = 'dbMatrix'),
  function(.Object,
           driver = NULL,
           connection = NULL,
           table_name = NULL,
           dim_names = NULL,
           dim = NULL) {

    if(!is.null(driver)) {
      .Object@driver = driver
    }

    if(!is.null(connection)) {
      .Object@connection = connection

      con_path = DBI::dbGetInfo(.Object@connection)$dbname
      .Object@path = normalizePath(con_path)
    }


    # Detect if object ready to use
    if(!is.na(table_name) & !is.null(connection)) {
      .Object@valid = TRUE
    }


    if(isTRUE(.Object@valid)) {

    }


    validObject(.Object)
    return(.Object)
  })





#' @param matrix object coercible to matrix or filepath to matrix data readable
#' by duckdb
#' @param db_name name to assign within database
#' @param db_path path to database on disk
#' @param db_driver db driver generation function
#' @noRd
create_dbMatrix = function(matrix,
                           db_name = 'test',
                           db_path = NULL,
                           db_driver = duckdb::duckdb) {

  # TODO read matrix
  if(is.character(matrix)) {

  }

  # TODO coerce matrix
  if(!inherits(matrix, 'matrix')) {

  }


  if(is.character(db_path)) {
    db_path = path.expand(db_path)
    if(!file.exists(db_path)) {
      dbdir = gsub(basename(db_path), '/', db_path)
      if(!dir.exists(dbdir)) dir.create(dbdir, recursive = TRUE)
    }

    con = DBI::dbConnect(drv = db_driver(),
                         dbdir = db_path)
  }




  # TODO edit dim_names and dim input to allow for workflows that do not read into mem
  dbMat = new('dbMatrix',
              driver = db_driver,
              connection = con,
              table_name = db_name,
              dim_names = list(rownames(matrix),
                               colnames(matrix)),
              dim = dim(matrix))


}


