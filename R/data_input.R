



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





# Other similar functions can be easily added by swapping out the repeat loop and
# colnames collection sections


#' @name streamToDB_fread
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
#' @param idx_col character. If not NULL, the specified column will be generated
#' as unique ascending integers
#' @param pk character. Which columns to use as primary key
#' @param nlines integer. Number of lines to read per chunk
#' @param cores fread cores to use
#' @param callback callback functions to apply to each data chunk before it is
#' sent to the database backend
#' @param overwrite whether to overwrite if table already exists (default = FALSE)
#' @param custom_table_fields default = NULL. If desired, custom setup the fields
#' of the table. See \code{\link[DBI]{dbCreateTable}}
#' @param ... additional params to pass to fread
#' @export
streamToDB_fread = function(path,
                            backend_ID,
                            remote_name = 'test',
                            idx_col = NULL,
                            pk = NULL,
                            nlines = 10000L,
                            cores = 1L,
                            callback = NULL,
                            overwrite = FALSE,
                            custom_table_fields = NULL,
                            ...) {
  checkmate::assert_file_exists(path)
  checkmate::assert_character(remote_name)
  checkmate::assert_numeric(nlines, len = 1L)
  if(!is.integer(nlines)) nlines = as.integer(nlines)
  if(!is.null(idx_col)) checkmate::assert_character(idx_col, len = 1L)
  if(!is.null(custom_table_fields)) stopifnot(is.character(custom_table_fields))
  p = evaluate_conn(backend_ID, mode = 'pool')


  # overwrite if necessary
  overwrite_handler(p = p, remote_name = remote_name, overwrite = overwrite)


  # get rows
  fext = file_extension(path)
  if('csv' %in% fext) file_format = 'csv'
  if('tsv' %in% fext) file_format = 'tsv'
  atab = arrow::open_dataset(path, format = file_format)
  n_rows = nrow(atab)

  # chunked reading
  chunk_num = idx_count = 0
  c_names_cache = colnames(data.table::fread(input = path, nrows = 0L))
  idx_list = NULL
  extab = FALSE
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
                  nThread = cores,
                  ...)

    # apply colnames
    data.table::setnames(chunk, new = c_names_cache)
    # apply callbacks
    if(!is.null(callback)) {
      stopifnot(is.function(callback))
      chunk = callback(chunk)
    }

    # index col generation
    if(!is.null(idx_col)) {
      if(idx_col %in% names(chunk)) {
        warning('idx_col already exists as a column.\n The unique index will not be generated')
        idx_col = NULL # set to null so warns only once
      } else {
        chunk[, (idx_col) := seq(.N) + idx_count]
        idx_count = chunk[, max(.SD), .SDcols = idx_col]
        data.table::setcolorder(chunk, idx_col)
      }
    }

    if(!extab) {
      # custom table creation
      # allows setting of specific data types (that do not conflict with data)
      # allows setting of keys and certain constraints
      createTableBE(
        conn = p,
        name = remote_name,
        fields_df = chunk,
        fields_custom = custom_table_fields,
        pk = pk,
        row.names = NULL,
        temporary = FALSE
      )
      extab = TRUE
    }

    # append data (create if necessary)
    pool::dbAppendTable(conn = p,
                        name = remote_name,
                        value = chunk)
  }

  return(invisible(idx_list))
}




# possible read methods: arrow, vect, rhdf5
#' @name streamSpatialToDB_arrow
#' @title Stream spatial data to database (arrow)
#' @param path path to database
#' @param backend_ID backend ID of database
#' @param remote_name name of table to generate in database backend
#' @param id_col ID column of data
#' @param xy_col character vector. Names of x and y value spatial coordinates or
#' vertices
#' @param extent terra SpatExtent (optional) that can be used to subset the data
#' to read in before it is saved to database
#' @param file_format (optional) file type (one of csv/tsv, arrow, or parquet)
#' @param chunk_size chunk size to use when reading in data
#' @param callback callback function to allow access to read-chunk-level data
#' formatting and filtering. Input and output should both be data.table
#' @param overwrite logical. whether to overwrite if table already exists in
#' database backend
#' @param custom_table_fields default = NULL. If desired, custom setup the fields
#' of the table. See \code{\link[DBI]{dbCreateTable}}
#' @param custom_table_fields_attr default = NULL. If desired, custom setup the
#' fields of the paired attributes table
#' @return invisibly returns the final geom ID used
streamSpatialToDB_arrow = function(path,
                                   backend_ID,
                                   remote_name = 'test',
                                   id_col = 'poly_ID',
                                   xy_col = c('x', 'y'),
                                   extent = NULL,
                                   file_format = NULL,
                                   chunk_size = 10000L,
                                   callback = NULL,
                                   overwrite = FALSE,
                                   custom_table_fields = NULL,
                                   custom_table_fields_attr = NULL,
                                   ...) {
  checkmate::assert_file_exists(path)
  checkmate::assert_character(remote_name, len = 1L)
  checkmate::assert_character(backend_ID, len = 1L)
  checkmate::assert_character(id_col, len = 1L)
  checkmate::assert_character(xy_col, len = 2L)
  checkmate::assert_numeric(chunk_size, len = 1L)
  if(!is.null(custom_table_fields)) checkmate::assert_character(custom_table_fields)
  if(!is.null(custom_table_fields_attr)) checkmate::assert_character(custom_table_fields_attr)

  p = getBackendPool(backend_ID = backend_ID)
  attr_name = paste0(remote_name, '_attr')

  # overwrite if necessary
  overwrite_handler(p = p, remote_name = remote_name, overwrite = overwrite)
  overwrite_handler(p = p, remote_name = attr_name, overwrite = overwrite)

  # custom table creation
  # allows setting of specific data types (that do not conflict with data)
  # allows setting of keys and certain constraints
  if(!is.null(custom_table_fields)) {
    DBI::dbCreateTable(
      conn = p,
      name = remote_name,
      fields = custom_table_fields,
      row.names = NA,
      temporary = FALSE
    )
  }
  if(!is.null(custom_table_fields_attr)) {
    DBI::dbCreateTable(
      conn = p,
      name = attr_name,
      fields = custom_table_fields_attr,
      row.names = NA,
      temporary = FALSE
    )
  }

  # determine compatible filetype
  if(is.null(file_format)) {
    fext = file_extension(path)
    if('csv' %in% fext) file_format = 'csv'
    if('tsv' %in% fext) file_format = 'tsv'
    if(any(c('pqt', 'parquet') %in% fext)) file_format = 'parquet'
    if(any(c('ipc', 'arrow', 'feather') %in% fext)) file_format = 'arrow'
  }


  # create file connection and get details
  x_col = xy_col[1]
  y_col = xy_col[2]
  atable = arrow::open_dataset(sources = path, format = file_format)
  # rename columns to standard names
  atable = atable %>%
    dplyr::rename(c(poly_ID = !!id_col,
                    x = !!x_col,
                    y = !!y_col))
  peek = atable %>% head(1) %>% dplyr::collect()
  c_names = colnames(peek)
  # include terra-needed columns
  if(!'part' %in% c_names) atable = atable %>% dplyr::mutate(part = 1L)
  if(!'hole' %in% c_names) atable = atable %>% dplyr::mutate(hole = 0L)

  # extent filtering
  if(!is.null(extent)) {
    atable = extent_filter(atable, extent = extent)
  }

  # chunked reading
  chunk_num = 0L
  pIDs = dplyr::distinct(atable, poly_ID) %>%
    dplyr::arrange(poly_ID) %>%
    dplyr::collapse()
  npoly = dplyr::pull(dplyr::tally(pIDs), as_vector = TRUE)
  repeat{

    chunk_num = chunk_num + 1L
    chunk_start = (chunk_num - 1L) * chunk_size + 1L
    chunk_end = min(chunk_size + chunk_start - 1L, npoly)
    if(chunk_start > npoly) {
      # end of file
      break
    }
    poly_select = pIDs[chunk_start:chunk_end,] %>%
      dplyr::collect() %>%
      dplyr::pull()

    chunk = atable %>%
      dplyr::filter(poly_ID %in% poly_select) %>%
      dplyr::collect() %>%
      data.table::setDT() %>%
      data.table::setkeyv('poly_ID')

    # callbacks
    if(!is.null(callback)) {
      checkmate::assert_function(callback)
      chunk = callback(chunk)
    }

    # setup geom column (polygon unique integer ID)
    nr_of_cells_vec = seq_along(poly_select) + chunk_start - 1L
    names(nr_of_cells_vec) = poly_select
    new_vec = nr_of_cells_vec[as.character(chunk$poly_ID)]
    chunk[, geom := new_vec]

    all_colnames = colnames(chunk)
    geom_values = c('geom', 'part', 'x', 'y', 'hole')
    attr_values = c('geom', all_colnames[!all_colnames %in% geom_values])

    spat_chunk = chunk[,geom_values, with = FALSE]
    attr_chunk = unique(chunk[,attr_values, with = FALSE])

    # append data (create if necessary)
    pool::dbWriteTable(conn = p,
                       name = remote_name,
                       value = spat_chunk,
                       append = TRUE,
                       temporary = FALSE)
    pool::dbWriteTable(conn = p,
                       name = attr_name,
                       value = attr_chunk,
                       append = TRUE,
                       temporary = FALSE)
  }
  return(invisible(max(nr_of_cells_vec)))
}





# chunk reading callbacks ####

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
#' @param group_by numeric or character designating which column of current data
#' to set as i. Default = 1st column
#' @return data.table in ijx format
#' @export
callback_formatIJX = function(x, group_by = 1) {
  assert_DT(x)

  x = data.table::melt(data = x, group_by, value.name = 'x')
  data.table::setnames(x, new = c('i', 'j', 'x'))
  x[, i := as.character(i)]
  x[, j := as.character(j)]
  x[, x := as.numeric(x)]
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








# data appending ####
# TODO




# @name writeAsGDB
# @title Write an object into the database backend as a GiottoDB object
# @description
# Given a matrix, data.frame-like object, or SpatVector polygon or points,
# write the values to database. Supports appending of information in case this
# data should be added iteratively into the database (such as when operations
# are parallelized). Returns a GiottoDB object of the analogous class.
# @param backend_ID backend_ID, or pool object with which to associate this object
# to a specific database backend
# @param x object to write
# @param append whether to append the values
# @param remote_name name to assign in the database
# @param ... additional params to pass
# setMethod('writeAsGDB', signature('data.frame'), function(backend_ID, x, append = FALSE, ...) {
#   p = evaluate_conn(backend_ID, mode = 'pool')
#   checkmate::assert_logical(append)
#
#   dbDataFrame(key = ,data = ,hash = ,remote_name = )
# })






# @name append_permanent
# @title Append values to database
# @description
# Writes values to database in an appending manner. As such, it does not check
# if there is already an existing table.
# @param p Pool connection object
# @param x data to append, given as a data.frame-like object
# @param remote_name name to assign the object on the database
# @param data_type type of data that is being appended to
# @param fields additional fields constraints that might be desired if the table
# is being newly generated
# @param ... additional params to pass to \code{\link[DBI]{dbCreateTable}}
# append_permanent = function(p,
#                             x,
#                             remote_name,
#                             data_type = c('matrix', 'df', 'polygon', 'points'),
#                             fields = NULL,
#                             ...) {
#   checkmate::assert_class(p, 'Pool')
#   checkmate::assert_character(remote_name)
#   checkmate::assert_character(data_type)
#
#   # create table if fields constraints are provided and table does not exist
#   if(!is.null(fields) & !existsTableBE(p, remote_name)) {
#     switch(data_type,
#            'matrix' = {
#              pool::dbCreateTable(conn = p,
#                                  name = remote_name,
#                                  fields = fields,
#                                  row.names = NA,
#                                  temporary = FALSE)
#            },
#            'df' = {
#              pool::dbCreateTable(conn = p,
#                                  name = remote_name,
#                                  fields = fields,
#                                  row.names = NA,
#                                  temporary = FALSE)
#            },
#            'polygon' = {
#              pool::dbCreateTable(conn = p,
#                                  name = remote_name,
#                                  fields = fields[[1L]],
#                                  row.names = NA,
#                                  temporary = FALSE)
#              if(!is.null(fields[[2L]])) {
#                pool::dbCreateTable(conn = p,
#                                    name = paste0(remote_name, '_attr'),
#                                    fields = fields[[2L]],
#                                    row.names = NA,
#                                    temporary = FALSE)
#              }
#            },
#            'points' = {
#              pool::dbCreateTable(conn = p,
#                                  name = remote_name,
#                                  fields = fields,
#                                  row.names = NA,
#                                  temporary = FALSE)
#            })
#   }
#
#   # append in data
#   # assumes input is from terra::geom() and terra::values()
#   switch(data_type,
#          'matrix' = {
#
#          },
#          'df' = {
#          },
#          'polygon' = {
#
#          },
#          'points' = {
#          })
# }








# internal function to stream specific data types into the database backend.
# if custom_table_fields is provided and the table does not yet exist then
# a specific DB table will be created with constraints. In all cases,
# values will be fed to `dbWriteTable` with `append = TRUE` so that the values
# will be appended, and if a specific table does not yet exist, it will be created.
# Right now this is mainly intended only for spatial objects since they are the
# only ones that require in-memory chunked processing
setMethod('stream_to_db', signature(p = 'Pool', remote_name = 'character', x = 'SpatVector'), function(p, remote_name, x, ...) {
  switch(terra::geomtype(x),
         'polygons' = append_permanent_dbpoly(p = p, remote_name = remote_name, SpatVector = x, ...),
         'points' = append_permanent_dbpoints(p = p, remote_name = remote_name, SpatVector = x, ...))
})



append_permanent_dbpoly = function(p,
                                   SpatVector,
                                   remote_name,
                                   start_index = NULL,
                                   custom_table_fields = NULL,
                                   custom_table_fields_attr = NULL,
                                   ...) {
  checkmate::assert_class(p, 'Pool')
  checkmate::assert_character(remote_name, len = 1L)
  checkmate::assert_class(SpatVector, 'SpatVector')
  checkmate::assert_true(terra::geomtype(SpatVector) == 'polygons')
  if(!is.null(custom_table_fields)) checkmate::assert_character(custom_table_fields)
  if(!is.null(custom_table_fields_attr)) checkmate::assert_character(custom_table_fields_attr)
  attr_name = paste0(remote_name, '_attr')

  # custom table creation
  # allows setting of specific data types (that do not conflict with data)
  # allows setting of keys and certain constraints
  if(!existsTableBE(x = p, remote_name = remote_name)) {
    if(!is.null(custom_table_fields)) {
      pool::dbCreateTable(
        conn = p,
        name = remote_name,
        fields = custom_table_fields,
        row.names = NA,
        temporary = FALSE
      )
    }
    if(!is.null(custom_table_fields_attr)) {
      pool::dbCreateTable(
        conn = p,
        name = attr_name,
        fields = custom_table_fields_attr,
        row.names = NA,
        temporary = FALSE
      )
    }
    start_index = 0L
  } else {
    start_index = sql_max(p, attr_name, 'geom')
  }


  # extract info
  geom_DT = terra::geom(SpatVector, df = TRUE) %>%
    data.table::setDT()
  atts_DT = terra::values(SpatVector) %>%
    data.table::setDT()
  # assign common geom col to atts table
  atts_DT[, geom := unique(geom_DT$geom)]

  # increment indices
  geom_DT[, geom := geom + start_index]
  atts_DT[, geom := geom + start_index]

  # append/write info
  pool::dbWriteTable(conn = p,
                     name = remote_name,
                     value = geom_DT,
                     append = TRUE,
                     temporary = FALSE)
  pool::dbWriteTable(conn = p,
                     name = attr_name,
                     value = atts_DT,
                     append = TRUE,
                     temporary = FALSE)
}




append_permanent_dbpoints = function(p,
                                     SpatVector,
                                     remote_name,
                                     custom_table_fields = NULL,
                                     ...) {
  checkmate::assert_class(p, 'Pool')
  checkmate::assert_character(remote_name, len = 1L)
  checkmate::assert_class(SpatVector, 'SpatVector')
  checkmate::assert_true(terra::geomtype(SpatVector) == 'points')
  if(!is.null(custom_table_fields)) checkmate::assert_character(custom_table_fields)

  # custom table creation
  # allows setting of specific data types (that do not conflict with data)
  # allows setting of keys and certain constraints
  if(!existsTableBE(x = p, remote_name = remote_name)) {
    if(!is.null(custom_table_fields)) {
      pool::dbCreateTable(
        conn = p,
        name = remote_name,
        fields = custom_table_fields,
        row.names = NA,
        temporary = FALSE
      )
    }
  }


  # extract info
  geom_DT = data.table::setDT(terra::geom(SpatVector, df = TRUE))
  atts_DT = data.table::setDT(terra::values(SpatVector))
  chunk = cbind(geom_DT[, .(x, y)], atts_DT)

  # append/write info
  pool::dbWriteTable(conn = p,
                     name = remote_name,
                     value = chunk,
                     append = TRUE,
                     temporary = FALSE)
}



setMethod(
  'stream_to_db',
  signature(p = 'Pool', remote_name = 'character', x = 'data.frame'),
  function(p, remote_name, x, custom_table_fields = NULL, ...) {
    checkmate::assert_class(p, 'Pool')
    checkmate::assert_character(remote_name, len = 1L)

    # custom table creation
    # allows setting of specific data types (that do not conflict with data)
    # allows setting of keys and certain constraints
    if(!existsTableBE(x = p, remote_name = remote_name)) {
      if(!is.null(custom_table_fields)) {
        pool::dbCreateTable(
          conn = p,
          name = remote_name,
          fields = custom_table_fields,
          row.names = NA,
          temporary = FALSE
        )
      }
    }

    # append/write info
    pool::dbWriteTable(conn = p,
                       name = remote_name,
                       value = x,
                       append = TRUE,
                       temporary = FALSE)
  })






