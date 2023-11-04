# Pipe magrittr ####

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
  wrap_txt('dbMatrix:', ..., errWidth = TRUE) %>%
    stop(call. = FALSE)
}


# From a vector, generate a string with the pattern 'item1', 'item2'
#' @keywords internal
#' @noRd
vector_to_string = function(x) {
  toString(sprintf("'%s'", x))
}

#' @title Generate array for pretty printing of matrix values
#' @name print_array
#' @param i,j,x matched vectors of integers in i and j, with value in x
#' @param dims dimensions of the array (integer vector of 2)
#' @param fill fill character
#' @param digits default = 5. If numeric, round to this number of digits
#' @keywords internal
print_array = function(i = NULL,
                       j = NULL,
                       x = NULL,
                       dims,
                       rownames = rep('', dims[1]),
                       fill = '.',
                       digits = 5L) {
  total_len = prod(dims)

  # pre-generate filler values
  a_vals = rep('.', total_len)

  ijx_nargs = sum(!is.null(i), !is.null(j), !is.null(x))
  if(ijx_nargs < 3 && ijx_nargs > 1) {
    stopf('All values for i, j, and x must be given when printing')
  }
  if(ijx_nargs == 3) {
    # format numeric
    if(is.numeric(x)) {
      ifelse(x < 1e4,
             format(x, digits = digits),
             format(x, digits = digits, scientific = TRUE))
    }
    # populate sparse values by replace nth elements
    # note that i and j index values must be determined outside of this function
    # since the colnames are not known in here
    for(n in seq_along(x)) {
      a_vals[ij_array_map(i = i[n], j = j[n], dims = dims)] <- x[n]
    }
  }

  # print array
  array(a_vals, dims, dimnames = list(rownames, rep('', dims[2]))) %>%
    print(quote = FALSE, right = TRUE)
}

# Map row (i) and col (j) indices to nth value of an array vector
#' @keywords internal
#' @noRd
#' @return integer position in array vector the i and j map to
ij_array_map = function(i, j, dims) {
  # arrays map vector values first by row then by col
  (j - 1) * dims[1] + i
}
