






# initialize ####

setMethod(
  'initialize',
  signature(.Object = 'dbMatrix'),
  function(.Object,
           connection = NULL,
           path = NULL,
           table_name = NULL,
           dim_names = NULL,
           dim = NULL) {


    if(!is.null(connection)) {
      .Object@connection = connection
    }


    if(!is.null(path)) {

      dbpath = path.expand(path)
      if(!file.exists(dbpath)) {
        dbdir = gsub(basename(dbpath), '/', dbpath)
        dir.create(dbdir, recursive = TRUE)
      }

      con = duckdb::dbConnect(duckdb::duckdb(),
                              dbdir = dbpath)

      if(!is.null(.Object@connection)) {
        message()
      }
      .Object@connection = con
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

