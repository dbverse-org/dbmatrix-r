


setMethod('initialize', signature('dbDataFrame'), function(.Object, ...) {

  # call dbData initialize
  .Object = methods::callNextMethod(.Object, ...)

  # check and return #
  # ---------------- #

  validObject(.Object)
  return(.Object)

})



# TODO compute_dbDataFrame_permanent
