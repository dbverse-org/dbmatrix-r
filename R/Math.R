
# https://github.com/cran/Matrix/blob/master/R/Math.R

Math.vecGenerics = grep('^cum', getGroupMembers('Math'), value = TRUE)

#' @include classes.R
#' @rdname hidden_aliases
#' @export
setMethod('Math', signature(x = 'dbMatrix'), function(x)
{
  if(!.Generic %in% Math.vecGenerics) {
    build_call = str2lang(paste0('x[] %>% dplyr::mutate(x = `',
                                 as.character(.Generic),
                                 '`(x))'))
    x[] = eval(build_call)
    print(x[])
  } else {
    stopf('Not currently supported\n')
  }
  x
})



