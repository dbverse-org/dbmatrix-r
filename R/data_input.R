



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




#' @title Read matrix via R and data.table
#' @name readMatrixDB
#' @description Function to read a matrix in from flat file directly into database.
#' This function is specific for tsv and csv style formats. The table will be read
#' in as 'read_temp' and converted to IJX format and permanently written in as a
#' table with the name supplied through remote_name param. The table generated
#' when reading ('read_temp') will then be dropped.
#' @param path path to the matrix file
#' @param backend_ID ID of backend to use
#' @param transpose transpose matrix
#' @param remote_name name to assign the dbMatrix
#' @param rowname_cols numeric vector. Which columns of flat file contain rownames
#' information
#' @param ... additional params to pass
#' @return matrix
#' @details The matrix needs to have both unique column names and row names.
#' A Giotto database backend also must already be existing. .csv and .tsv files
#' when the database is duckdb will be handled through duckdb_read_csv
#' @export
readMatrixDB = function(path,
                        backend_ID,
                        remote_name = 'temp',
                        rowname_cols = 1,
                        transpose = FALSE,
                        ...) {
  stopifnot(is.character(remote_name))
  stopifnot(is.character(backend_ID))
  stopifnot(file.exists(path))

  conn = getBackendConn(backend_ID = backend_ID)
  on.exit(pool::poolReturn(conn))
  fnq = get_full_table_name_quoted(conn = conn, remote_name = remote_name)


  # 1. get rownames and colnames
  c_names = path %>%
    data.table::fread(nrows = 0L, drop = rowname_cols) %>%
    colnames()

  r_names_table = path %>%
    data.table::fread(select = rowname_cols)
  if(length(rowname_cols) > 1L) {
    r_names_table[, combined := do.call(paste, c())] #TODO
  } else {

  }


  # 2. read in flat file

  streamToDB(path = path,
             backend_ID = backend_ID,
             remote_name = 'read_temp')

  # 3. convert to long format ijx

  # account for multiple ID related columns

  temp_table = dplyr::tbl(conn, 'read_temp') %>%
    tidyr::pivot_longer(cols = !1, names_to = 'j', values_to = 'x') |>
    dplyr::rename(i = 1)


  # 4. save resulting table as permanent
  sql_create = create_dbmatrix_sql(conn = conn, full_name_quoted = fnq)
  DBI::dbExecute(conn = conn, sql_create)
  DBI::dbAppendTable(conn = conn,
                     name = remote_name,
                     value = temp_table,
                     ...)

  DBI::dbRemoveTable(conn, 'read_temp')


  return()
}





#' @name pivotToIJX
#' @title Pivot a remote table to IJX format
#' @param x a remote database table
#' @export
pivotToIJX = function(x) {
  assert_conn_table(x)
  assert_conn_valid(x)

  # Split data into smaller chunks
  chunk_size <- 10000
  num_chunks <- ceiling(nrow(x) / chunk_size)
  chunks <- split(x, 0:(num_chunks - 1) %/% chunk_size)

  # Apply pivot_longer to each chunk
  chunks <- purrr::map(chunks, ~ .x %>%
                         tidyr::pivot_longer(cols = !1, names_to = 'j', values_to = 'x') %>%
                         dplyr::rename(i = 1))

  # Combine chunks back into a single data frame
  x <- do.call(rbind, chunks)

  return(x)
}




#' @name fstreamToDB_IJX
#' @title Stream large flat filess to database backend using fread
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
fstreamToDB_IJX = function(path,
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

  # create new ijx table
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






# Old
#' streamToDB = function(path,
#'                       backend_ID,
#'                       remote_name = 'read_temp',
#'                       ...) {
#'   stopifnot(is.character(remote_name))
#'   stopifnot(is.character(backend_ID))
#'   stopifnot(file.exists(path))
#'
#'   conn = getBackendConn(backend_ID = backend_ID)
#'   on.exit(pool::poolReturn(conn))
#'
#'   if(match_file_ext(path = path, ext_to_match = '.csv')) { # .csv
#'     if(dbms(conn) == 'duckdb') {
#'       duckdb::duckdb_read_csv(conn = conn,
#'                               name = remote_name,
#'                               files = path,
#'                               delim = ',',
#'                               header = TRUE,
#'                               ...)
#'     } else {
#'       arkdb::unark(files = path,
#'                    db_con = conn,
#'                    tablenames = remote_name,
#'                    overwrite = TRUE,
#'                    ...)
#'     }
#'
#'   } else if(match_file_ext(path = path, ext_to_match = '.tsv')) { # .tsv
#'
#'     arkdb::unark(files = path,
#'                  db_con = conn,
#'                  tablenames = remote_name,
#'                  overwrite = TRUE,
#'                  ...)
#'
#'   } else { # else throw error
#'     stopf('readMatrixDB only works for .csv and .tsv type files
#'           Not:', basename(path))
#'   }
#' }



#' @name combineColsDB
#' @title Combine designated columns in database table
#' @description Combines designated columns into a single one and removes the
#' originals. Useful for wrangling ID column values. The combined column will
#' be created as a new column called 'combined_col'
#' @param backend_ID ID of backend
#' @param remote_name name of table on database
#' @param col_indices numerical vector of columns to combine
#' @param remove_originals boolean. Whether to remove the original columns
#' @export
combineColsDB = function(x,
                         col_indices,
                         combined_colname = 'new',
                         remove_originals = TRUE) {
  assert_conn_table(x)
  stopifnot(is.numeric(col_indices))

  col_n = colnames(x)[col_indices]
  .col_n = col_n %>%
    sapply(as.name, USE.NAMES = FALSE) %>%
    as.list()

  x = x %>%
    dplyr::mutate(!!combined_colname := paste0('ID_', paste(!!!.col_n, sep = '_')))

  if(isTRUE(remove_originals)) {
    select_cols = colnames(x)[-c(1,2)][c(ncol(x) - 2, seq(ncol(x) - 3))]
    x = x %>%
      dplyr::select(tidyr::all_of(select_cols))
  }

  return(x)
}




# TODO read .mtx format (uses dictionaries and has no 0 values. Already in ijx)

# TODO read .parquet (arrow::read_parquet(), arrow::to_duckdb(), tidyr::pivot_longer())




match_file_ext = function(path, ext_to_match) {
  stopifnot(file.exists(path))

  grepl(pattern = paste0(ext_to_match, '$|', ext_to_match, '\\.'),
        x = path,
        ignore.case = TRUE)
}


