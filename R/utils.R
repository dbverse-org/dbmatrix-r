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





#' @keywords internal
#' @noRd
set_db_path = function(path = ':temp:', extension = '.duckdb') {
  stopifnot(is.character(path))
  if(path == ':memory:') return(path)
  parent = switch(path,
                  ':temp:' = tempdir(),
                  ':memory' = ':memory:',
                  path)
  parent = normalizePath(parent)
  if(!dir.exists(parent)) {
    dir.create(dir.create(parent, recursive = TRUE))
  }

  file.path(parent, paste0('giotto_backend', extension))
}



#' @name calculate_hash
#' @title Calculate a hash value
#' @description
#' Generate a hash value for x
#' @param object anything
#' @keywords internal
calculate_hash = function(object) {
  digest::digest(object = object, algo = 'xxhash64')
}

