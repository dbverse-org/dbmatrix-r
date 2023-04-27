#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' @importFrom data.table ":="
NULL





# Print Formatting ####

#' @title Wrap message
#' @name wrap_msg
#' @param ... additional strings and/or elements to pass to wrap_txt
#' @param sep how to join elements of string (default is one space)
#' @keywords internal
#' @noRd
wrap_msg = function(..., sep = ' ') {
  message(wrap_txt(..., sep = sep))
}

#' @title Wrap text
#' @name wrap_txt
#' @param ... additional params to pass
#' @param sep how to join elements of string (default is one space)
#' @param strWidth externally set wrapping width. (default value of 100 is not effected)
#' @param errWidth default = FALSE. Set strWidth to be compatible with error printout
#' @keywords internal
#' @noRd
wrap_txt = function(..., sep = ' ', strWidth = 100, errWidth = FALSE) {
  custom_width = ifelse(is.null(match.call()$strWidth), yes = FALSE, no = TRUE)
  if(!isTRUE(custom_width)) {
    if(isTRUE(errWidth)) strWidth = getOption('width') - 6
  }

  cat(..., sep = sep) %>%
    capture.output() %>%
    strwrap(., prefix =  ' ', initial = '', # indent later lines, no indent first line
            width = min(80, getOption("width"), strWidth)) %>%
    paste(., collapse = '\n')
}



# Custom stop function
stopf = function(...) {
  wrap_txt('GiottoDB:', ..., errWidth = TRUE) %>%
    stop(call. = FALSE)
}


# From a vector, generate a string with the pattern 'item1', 'item2'
#' @keywords internal
#' @noRd
vector_to_string = function(x) {
  toString(sprintf("'%s'", x))
}






#' @name getDBPath
#' @title Get the database backend path
#' @description
#' Get the full normalized filepath of a Giotto backend database. Additionally
#' passing path = ':memory:' will directly return ':memory:' and ':temp:' will
#' will have the function check tempdir() for the backend.
#' @param path directory path in which to place the backend
#' @param extension file extension of the backend (default is .duckdb)
#' @export
getDBPath = function(path = ':temp:', extension = '.duckdb') {
  stopifnot(is.character(path))
  if(path == ':memory:') return(path)
  parent = switch(path,
                  ':temp:' = tempdir(),
                  path)
  parent = normalizePath(parent, mustWork = FALSE)

  if(!basename(parent) == paste0('giotto_backend', extension)) {
    full_path = file.path(parent, paste0('giotto_backend', extension))
  } else {
    full_path = parent
  }
  full_path = normalizePath(full_path, mustWork = FALSE)

  if(!file.exists(full_path)) {
    stopf('No Giotto backend found at\n',
          full_path,
          '\nPlease create or restart the backend.')
  }

  return(full_path)
}



# Internal function. Given a path and extension, create a full path that ends in
# 'giotto_backend[extension]'.
# The path is normalized to ensure relative path param inputs have
# the same outputs as the absolute path.
#
# If the directory in which to place the database does not exist, then it will
# be generated.
#' @keywords internal
#' @noRd
set_db_path = function(path = ':temp:', extension = '.duckdb', verbose = TRUE) {
  stopifnot(is.character(path))
  if(path == ':memory:') return(path)
  parent = switch(path,
                  ':temp:' = tempdir(),
                  path)
  parent = normalizePath(parent, mustWork = FALSE)

  # get dir if already full path
  if(basename(parent) == paste0('giotto_backend', extension)) {
    parent = gsub(pattern = basename(parent),
                  replacement = '',
                  x = parent)
  }
  parent = normalizePath(parent, mustWork = FALSE)

  if(!dir.exists(parent)) {
    dir.create(dir.create(parent, recursive = TRUE))
  }

  full_path = file.path(parent, paste0('giotto_backend', extension))
  if(isTRUE(verbose)) wrap_msg('Creating backend at\n', full_path)
  return(full_path)
}





#' @name getBackendID
#' @title Get the backend hash ID from the database path
#' @param path directory path to the database. Accepts :memory: and :temp:
#' inputs as well
#' @param extension file extension of database backend (default = '.duckdb')
#' @export
getBackendID = function(path = ':temp:', extension = '.duckdb') {
  full_path = getDBPath(path = path, extension = extension)
  hash = calculate_backend_id(full_path)
  return(hash)
}





#' @name calculate_backend_id
#' @title Calculate a backend ID
#' @description Calculate a GiottoDB backend ID from its filepath. A hash value
#' is calculate and pre-pended with 'ID_' for clarity.
#' @param path filepath to database backend
#' @keywords internal
#' @noRd
calculate_backend_id = function(path) {
  hash = calculate_hash(path)
  return(paste0('ID_', hash))
}




#' @name calculate_hash
#' @title Calculate a hash value
#' @description
#' Generate a hash value for x
#' @param object anything
#' @keywords internal
#' @noRd
calculate_hash = function(object) {
  digest::digest(object = object, algo = 'xxhash64')
}





