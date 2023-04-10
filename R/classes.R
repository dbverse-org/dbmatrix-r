

# dbData Class ####

#' @name dbData
#' @title dbData
#' @description Framework for objects that link to a database
#' @slot dvr_call driver function for the database as a call
#' @slot custom_call custom calls used to generate or regenerate a database
#' connection with the correct formatting
#' @slot data dplyr tbl that represents the database data
#' @slot path path to database on-disk file
#' @slot remote_name name of table within database that contains the data
#' @slot dim_names row [1] and col [2] names
#' @slot dim dimensions of the matrix
#' @slot connect_type type of connection (either default or custom)
#' @noRd
setClass('dbData',
         contains = 'VIRTUAL',
         slots = list(
           dvr_call = 'ANY',
           custom_call = 'ANY',
           data = 'ANY',
           path = 'character',
           remote_name = 'character',
           connect_type = 'character'
         ),
         prototype = list(
           dvr_call = NULL,
           data = NULL,
           path = NA_character_,
           remote_name = NA_character_,
           connect_type = NA_character_
         ))




# dbMatrix Class ####

#' @noRd
check_dbMatrix = function(object) {
  errors = character()

  if(!is.na(object@connect_type)) {
    if(!object@connect_type %in% c('default', 'custom')) {
      msg = 'Invalid connect_type. Must be either "default" or "custom"\n'
      errors = c(errors, msg)
    }


    if(object@connect_type == 'default') {
      if(is.na(object@remote_name)) {
        msg = 'Name of table within DB to link to must be supplied\n use remoteName()<- to set\n'
        errors = c(errors, msg)
      }
    }

    if(object@connect_type == 'custom') {
      if(is.null(custom_call)) {
        msg = 'custom_call is missing.\n'
        errors = c(errors, msg)
      }
    }
  }

  if(is.null(object@data)) {
    msg = 'Missing DB connection table\n Run initialize() on this object\n'
    errors = c(errors, msg)
  } else if(!inherits(object@data, 'tbl_dbi')) {
    msg = 'Only class of "tbl_dbi" allowed in data slot\n'
    errors = c(errors, msg)
  } else if(!DBI::dbIsValid(object@data$src$con)) {
    msg = 'Invalid or disconnected DB connection\n Run reconnect() on this object\n'
    errors = c(errors, msg)
  }

  if(is.null(object@dim_names)) {
    # dim names are needed as long as i and j are character
    msg = 'dim_names must be provided for dbMatrix'
    errors = c(errors, msg)
  }


  if(length(errors) == 0) TRUE else errors
}




#' @title S4 dbMatrix class
#' @description
#' Representation of triplet matrices using an on-disk database. Each object
#' is used as a connection to a single table that exists within the database.
#' @slot driver driver function for the database
#' @slot connection database connection object
#' @slot path path to database on-disk file
#' @slot table_name name of table within database that contains matrix
#' @slot dim_names row [1] and col [2] names
#' @slot dim dimensions of the matrix
#' @slot valid whether the object is initialized for use
#' @export
dbMatrix = setClass(
  'dbMatrix',
  contains = 'dbData',
  slots = list(
    dim_names = 'list',
    dim = 'integer'
  ),
  prototype = list(
    dim_names = list(NULL, NULL),
    dim = c(NA_integer_, NA_integer_)
  ),
  validity = check_dbMatrix
)





setMethod('show', signature(object = 'dbMatrix'), function(object) {

  dimn = slot(object, 'dim_names')
  rown = dimn[[1]]
  coln = dimn[[2]]

  # class and dim #
  # ------------- #

  if(is.null(nrow(object@data))) {
    cat('0 x 0 matrix of class "dbMatrix"\n')
    return() # exit early if no info
  } else {
    cat(object@dim[[1]], 'x', object@dim[[2]], ' matrix of class "dbMatrix"\n')
  }


  # preview print #
  # ------------- #

  # colnames
  colname_show_n = object@dim[[2]] - 6L
  if(colname_show_n < 0L) {
    message('Colnames: ', vector_to_string(coln))
  } else if(colname_show_n >= 1L) {
    message(
      '[[ Colnames ',
      vector_to_string(head(coln, 3L)),
      ' ... suppressing ', colname_show_n, ' ...',
      vector_to_string(tail(coln, 3L)),
      ' ]]'
    )
  }

  # matrix
  p_coln = head(coln, 10L)
  if(object@dim[[1L]] - 6L > 0L) {
    p_rown = c(head(rown, 3L), tail(rown, 3L))
  } else {
    p_rown = rown
  }

  preview_dt = object@data %>%
    dplyr::filter(i %in% p_rown & j %in% p_coln) %>%
    tidyr::pivot_wider(names_from = 'j', values_from = 'x') %>%
    data.table::as.data.table()
  colnames(preview_dt) = NULL

  if(nrow(preview_dt < 7L)) {
    print(preview_dt, digits = 5L, row.names = 'none')
  } else {
    print(preview_dt[1:3,], digits = 5L, row.names = 'none')

    sprintf(' ........suppressing %d columns and %d rows\n',
            object@dim[[2L]] - 10L, object@dim[[1L]] - 6L)

    print(preview_dt[4:6,], digits = 5L, row.names = 'none')
  }

  cat('\n')

})






# dbDataFrame Class ####


#' @title S4 dbDataFrame class
#' @description
#' Representation of dataframes using an on-disk database. Each object
#' is used as a connection to a single table that exists within the database.
#' @slot driver driver function for the database
#' @slot connection database connection object
#' @slot path path to database on-disk file
#' @slot table_name name of table within database that contains matrix
#' @slot valid whether the object is initialized for use
#' @export
dbDataFrame = setClass(
  'dbDataFrame',
  contains = 'dbData'
)
