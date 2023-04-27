



# backendInfo Class ####
#' @name backendInfo
#' @title backendInfo
#' @description
#' Simple S4 class to contain information about the database backend and
#' regenerate connection pools. A hash is generated from the db_path to act
#' as a unique identifier for each backend.
#' @slot driver_call DB driver call stored as a string
#' @slot db_path path to database
#' @slot hash xxhash64 hash of the db_path
#' @export
setClass('backendInfo',
         slots = list(
           driver_call = 'character',
           db_path = 'character',
           hash = 'character'
         ),
         prototype = list(
           driver_call = NA_character_,
           db_path = NA_character_,
           hash = NA_character_
         ))








# dbData Class ####

#' @name dbData
#' @title dbData
#' @description Framework for objects that link to the database backend
#' @slot data dplyr tbl that represents the database data
#' @slot hash unique hash ID for backend
#' @slot remote_name name of table within database that contains the data
#' @noRd
setClass('dbData',
         contains = 'VIRTUAL',
         slots = list(
           data = 'ANY',
           hash = 'character',
           remote_name = 'character'
         ),
         prototype = list(
           data = NULL,
           hash = NA_character_,
           remote_name = NA_character_
         ))




# dbMatrix Class ####

# check_dbMatrix = function(object) {
#   errors = character()
#
#   if(!is.na(object@connect_type)) {
#     if(!object@connect_type %in% c('default', 'custom')) {
#       msg = 'Invalid connect_type. Must be either "default" or "custom"\n'
#       errors = c(errors, msg)
#     }
#
#
#     if(object@connect_type == 'default') {
#       if(is.na(object@remote_name)) {
#         msg = 'Name of table within DB to link to must be supplied\n use remoteName()<- to set\n'
#         errors = c(errors, msg)
#       }
#     }
#
#     if(object@connect_type == 'custom') {
#       if(is.null(custom_call)) {
#         msg = 'custom_call is missing.\n'
#         errors = c(errors, msg)
#       }
#     }
#   }
#
#   if(is.null(object@data)) {
#     msg = 'Missing DB connection table\n Run initialize() on this object\n'
#     errors = c(errors, msg)
#   } else if(!inherits(object@data, 'tbl_dbi')) {
#     msg = 'Only class of "tbl_dbi" allowed in data slot\n'
#     errors = c(errors, msg)
#   } else if(!DBI::dbIsValid(object@data$src$con)) {
#     msg = 'Invalid or disconnected DB connection\n Run reconnect() on this object\n'
#     errors = c(errors, msg)
#   }
#
#   if(is.null(object@dim_names)) {
#     # dim names are needed as long as i and j are character
#     msg = 'dim_names must be provided for dbMatrix'
#     errors = c(errors, msg)
#   }
#
#
#   if(length(errors) == 0) TRUE else errors
# }




#' @title S4 dbMatrix class
#' @description
#' Representation of triplet matrices using an on-disk database. Each object
#' is used as a connection to a single table that exists within the database.
#' @slot data dplyr tbl that represents the database data
#' @slot hash unique hash ID for backend
#' @slot remote_name name of table within database that contains the data
#' @slot path path to database on-disk file
#' @slot dim_names row [1] and col [2] names
#' @slot dims dimensions of the matrix
#' @export
dbMatrix = setClass(
  'dbMatrix',
  contains = 'dbData',
  slots = list(
    dim_names = 'list',
    dims = 'integer'
  ),
  prototype = list(
    dim_names = list(NULL, NULL),
    dims = c(NA_integer_, NA_integer_)
  )
)





setMethod('show', signature(object = 'dbMatrix'), function(object) {

  dimn = slot(object, 'dim_names')
  rown = dimn[[1]]
  coln = dimn[[2]]

  # class and dim #
  # ------------- #

  if(identical(object@dims,  c(0L, 0L))) {
    cat('0 x 0 matrix of class "dbMatrix"\n')
    return() # exit early if no info
  } else {
    cat(object@dims[[1]], 'x', object@dims[[2]], ' matrix of class "dbMatrix"\n')
  }


  # preview print #
  # ------------- #

  # colnames
  colname_show_n = object@dims[[2]] - 6L
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
  if(object@dims[[1L]] - 6L > 0L) {
    p_rown = c(head(rown, 3L), tail(rown, 3L))
  } else {
    p_rown = rown
  }

  preview_dt = object@data %>%
    dplyr::filter(i %in% p_rown & j %in% p_coln) %>%
    dplyr::mutate(j = paste0("col_", j)) %>% # forces type to be character
    tidyr::pivot_wider(names_from = 'j', values_from = 'x') %>%
    data.table::as.data.table()
  data.table::setkeyv(preview_dt, names(preview_dt)[1L]) # enforce ordering
  colnames(preview_dt) = NULL

  if(nrow(preview_dt < 7L)) {
    print(preview_dt, digits = 5L, row.names = 'none')
  } else {
    print(preview_dt[1:3,], digits = 5L, row.names = 'none')

    sprintf(' ........suppressing %d columns and %d rows\n',
            object@dims[[2L]] - 10L, object@dims[[1L]] - 6L)

    print(preview_dt[4:6,], digits = 5L, row.names = 'none')
  }

  cat('\n')

})






# dbDataFrame Class ####


#' @title S4 dbDataFrame class
#' @description
#' Representation of dataframes using an on-disk database. Each object
#' is used as a connection to a single table that exists within the database.
#' @slot data dplyr tbl that represents the database data
#' @slot hash unique hash ID for backend
#' @slot remote_name name of table within database that contains the data
#' @export
dbDataFrame = setClass(
  'dbDataFrame',
  contains = 'dbData'
)
