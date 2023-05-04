



#' @title Read matrix via R and data.table
#' @name readMatrixDT
#' @description Function to read a matrix in from flat file via R and data.table
#' @param path path to the matrix file
#' @param cores number of cores to use
#' @param transpose transpose matrix
#' @return matrix
#' @details The matrix needs to have both unique column names and row names
#' @export
readMatrixDT = function(path,
                       cores = 1L,
                       transpose = FALSE) {

  # check if path is a character vector and exists
  if(!is.character(path)) stopf('path needs to be character vector')
  path = path.expand(path)
  if(!file.exists(path)) stopf('the file: ', path, ' does not exist')


  data.table::setDTthreads(threads = cores)

  # read and convert
  DT = suppressWarnings(data.table::fread(input = path, nThread = cores))
  mtx = as.matrix(DT[,-1])



  if(isTRUE(transpose)) {
    mtx = Matrix::Matrix(data = mtx,
                         dimnames = list(DT[[1]], colnames(DT[,-1])),
                         sparse = TRUE)
    mtx = Matrix::t(mtx)
  }

  return(mtx)
}







#' @name fstreamToDB
#' @title Stream large flat files to database backend using fread
#' @description
#' Files are read in chunks of lines via \code{fread} and then converted to the
#' required formatting with plugin functions provided through the \code{callback}
#' param before being written/appended to the database table.
#' This is slower than directly writing the information in, but is a scalable
#' approach as it never requires the full dataset to be in memory. \cr
#' If more than one or a custom callback is needed for the formatting then a
#' combined or new function can be defined on the spot as long as it accepts
#' \code{data.table} input and returns a \code{data.table}.
#' @param path path to the matrix file
#' @param backend_ID ID of the backend to use
#' @param remote_name name to assign table of read in values in database
#' @param nlines integer. Number of lines to read per chunk
#' @param cores fread cores to use
#' @param callback callback functions to apply to each data chunk before it is
#' sent to the database backend
#' @param overwrite whether to overwrite if table already exists (default = FALSE)
#' @param with_pk whether to generate a primary key on i and j. Warning size will
#' increase greatly with a pk
#' @param ... additional params to pass
#' @export
fstreamToDB = function(path,
                       backend_ID,
                       remote_name = 'test',
                       nlines = 10000L,
                       cores = 1L,
                       callback = NULL,
                       overwrite = FALSE,
                       with_pk = FALSE,
                       ...) {
  stopifnot(is.character(remote_name))
  stopifnot(is.character(backend_ID))
  stopifnot(is.numeric(nlines), length(nlines) == 1L)
  if(!is.integer(nlines)) nlines = as.integer(nlines)
  stopifnot(file.exists(path))

  p = getBackendPool(backend_ID = backend_ID)
  fnq = get_full_table_name_quoted(conn = p, remote_name = remote_name)

  # overwrite if necessary
  if(existsTableBE(x = p, remote_name = remote_name)) {
    if(isTRUE(overwrite)) {
      DBI::dbRemoveTable(p, DBI::SQL(fnq))
    }
    else {
      stopf(fnq, 'already exists.
          Set overwrite = TRUE to recreate it.')
    }
  }

  # create new ijx table with primary key if desired
  if(isTRUE(with_pk)) {
    sql_create = create_dbmatrix_sql(conn = p, full_name_quoted = fnq)
    DBI::dbExecute(conn = p, sql_create)
  }

  chunk_num = 0
  n_rows = fpeek::peek_count_lines(path = path)
  c_names_cache = data.table::fread(input = path, nrows = 0L) %>%
    colnames()
  repeat {
    chunk_num = chunk_num + 1
    nskip = (chunk_num - 1) * nlines + 1
    if(nskip >= n_rows) {
      # end of file
      break
    }

    chunk = data.table::fread(input = path,
                  nrows = nlines,
                  skip = nskip,
                  header = FALSE,
                  nThread = cores)

    # apply colnames
    data.table::setnames(chunk, new = c_names_cache)
    # apply callbacks
    if(!is.null(callback)) {
      stopifnot(is.function(callback))
      chunk = callback(chunk)
    }

    pool::dbWriteTable(conn = p,
                       name = fnq,
                       value = chunk,
                       append = TRUE,
                       temporary = FALSE)
  }

}





#' @name callback_combineCols
#' @title Combine columns values
#' @param x data.table
#' @param col_indices numeric vector. Col indices to combine
#' @param new_col name of new combined column
#' @param remove_originals remove originals cols of the combined col
#' default = TRUE
#' @return data.table with specified column combined
#' @export
callback_combineCols = function(x,
                                col_indices,
                                new_col = 'new',
                                remove_originals = TRUE) {
  if(missing(col_indices)) return(x)
  assert_DT(x)

  selcol = colnames(x)[col_indices]
  x[, (new_col) := do.call(paste, c('ID', .SD, sep = '_')), .SDcols = selcol]
  data.table::setcolorder(x, new_col)
  if(isTRUE(remove_originals)) {
    if(new_col %in% selcol) selcol = selcol[-which(selcol == new_col)]
    x[, (selcol) := NULL]
  }

  return(x)
}




#' @name callback_formatIJX
#' @title Convert table to ijx
#' @description
#' Building block function intended for use as or in callback functions used
#' when streaming large flat files to database. Converts the input data.table
#' to long format with columns i, j, and x where i and j are row and col
#' names and x is values. Columns i and j are additionally set as 'character'.
#' @param x data.table
#' @param group_by numeric or character designating which column to set as i.
#' Default = 1st column
#' @return data.table in ijx format
#' @export
callback_formatIJX = function(x, group_by = 1) {
  assert_DT(x)

  x = data.table::melt(data = x, group_by, value.name = 'x')
  data.table::setnames(x, new = c('i', 'j', 'x'))
  x[, i := as.character(i)]
  x[, j := as.character(j)]
  return(x)
}



#' @name callback_swapCols
#' @title Swap values in two columns
#' @param x data.table
#' @param c1 col 1 to use (character)
#' @param c2 col 2 to use (character)
#' @return data.table with designated column values swapped
#' @export
callback_swapCols = function(x, c1, c2) {
  assert_DT(x)
  if(identical(c1, c2)) stop('Cols to swap can not be identical')

  x[, c(c2, c1) := .(get(c1), get(c2))]
  return(x)
}




# Old
# streamToDB = function(path,
#                       backend_ID,
#                       remote_name = 'read_temp',
#                       ...) {
#   stopifnot(is.character(remote_name))
#   stopifnot(is.character(backend_ID))
#   stopifnot(file.exists(path))
#
#   conn = getBackendConn(backend_ID = backend_ID)
#   on.exit(pool::poolReturn(conn))
#
#   if(match_file_ext(path = path, ext_to_match = '.csv')) { # .csv
#     if(dbms(conn) == 'duckdb') {
#       duckdb::duckdb_read_csv(conn = conn,
#                               name = remote_name,
#                               files = path,
#                               delim = ',',
#                               header = TRUE,
#                               ...)
#     } else {
#       arkdb::unark(files = path,
#                    db_con = conn,
#                    tablenames = remote_name,
#                    overwrite = TRUE,
#                    ...)
#     }
#
#   } else if(match_file_ext(path = path, ext_to_match = '.tsv')) { # .tsv
#
#     arkdb::unark(files = path,
#                  db_con = conn,
#                  tablenames = remote_name,
#                  overwrite = TRUE,
#                  ...)
#
#   } else { # else throw error
#     stopf('readMatrixDB only works for .csv and .tsv type files
#           Not:', basename(path))
#   }
# }







# TODO read .mtx format (uses dictionaries and has no 0 values. Already in ijx)

# TODO read .parquet (arrow::read_parquet(), arrow::to_duckdb(), tidyr::pivot_longer())




# match_file_ext = function(path, ext_to_match) {
#   stopifnot(file.exists(path))
#
#   grepl(pattern = paste0(ext_to_match, '$|', ext_to_match, '\\.'),
#         x = path,
#         ignore.case = TRUE)
# }


