


setMethod('initialize', signature('dbDataFrame'), function(.Object, ...) {

  # call dbData initialize
  .Object = methods::callNextMethod(.Object, ...)

  # check and return #
  # ---------------- #

  validObject(.Object)
  return(.Object)

})





#' @title Create a dataframe with database backend
#' @name createDBDataFrame
#' @param df object coercible to matrix or filepath to matrix data accessible
#' by one of the read functions. Can also be a pre-prepared tbl_sql to compatible
#' database table
#' @param remote_name name to assign within database
#' @param db_path path to database on disk
#' @param overwrite whether to overwrite if table already exists in database
#' @param cores number of cores to use if reading into database
#' @param nlines number of lines to read per chunk if reading into database
#' @param callback callback functions to apply to each data chunk before it is
#' sent to the database backend
#' @param ... additional params to pass
#' @details Information is only read into the database during this process. Based
#' on the \code{remote_name} and \code{db_path} a lazy connection is then made
#' downstream during \code{dbData} initialization and appended to the object.
#' If a dplyr tbl is provided as pre-made input then it is evaluated for whether
#' it is a \code{tbl_Pool} and whether the table exists within the specified
#' backend then directly passed downstream.
#' @export
createDBDataFrame = function(df,
                             remote_name = 'df_test',
                             db_path = ':temp:',
                             overwrite = FALSE,
                             cores = 1L,
                             nlines = 10000L,
                             callback = NULL,
                             ...) {

  db_path = getDBPath(db_path)
  backend_ID = calculate_backend_id(db_path)
  p = getBackendPool(hash)
  if(inherits(df, 'tbl')) assert_in_backend(x = df, p = p)

  data = evaluate_dbdataframe(input = df,
                              p = p,
                              backend_ID = backend_ID,
                              remote_name = remote_name,
                              nlines = nlines,
                              cores = cores,
                              callback = callback,
                              overwrite = overwrite)

  new('dbDataFrame',
      data = data,
      hash = backend_ID,
      remote_name = remote_name)
}





#' @name evaluate_dbdataframe
#' @title Evaluate input for dbDataFrame creation
#' @param input input to evaluate for dbDataFrame
#' @param p Pool object connected to backend
#' @param backend_ID backend_ID
#' @param remote_name name of table in backend
#' @param nlines number of lines to read per chunk if reading from flat file
#' @param cores number of cores to use during reading
#' @param callback callback actions to perform
#' @param overwrite logical. Default = FALSE whether to overwrite if object to
#' create already exists in database by the same name and the input is NOT a
#' database connection dplyr tbl
#' @return tbl_Pool if input was valid pre-made connection tbl
#' @keywords internal
#' @noRd
evaluate_dbdataframe = function(input, p, backend_ID, remote_name, nlines,
                                cores, callback, overwrite = FALSE) {
  data = NULL
  if(inherits(input, 'tbl_Pool')) { # data is already in DB and tbl is provided
    data = input
  } else { # data must be read in

    # database input #
    overwrite_handler(p = p, remote_name = remote_name, overwrite = overwrite)

    # read input if needed
    if(is.character(input)) {
      fstreamToDB(path = input,
                  backend_ID = backend_ID,
                  remote_name = remote_name,
                  # indices = c('i', 'j'),
                  nlines = nlines,
                  with_pk = FALSE,
                  cores = cores,
                  callback = callback,
                  overwrite = overwrite)
    }
  }
  data
}




# TODO compute_dbDataFrame_permanent
