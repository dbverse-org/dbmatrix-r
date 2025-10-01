# dbData ####
## Empty ####
### Extract [] ####
# Inherit from dbData class in {dbProject}

### Set [] ####
# Inherit from dbData class in {dbProject}

# dbMatrix ####
## vector indexing ####
### rows only ####
#' @noRd
#' @concept dbMatrix
#' @export
setMethod(
  '[',
  signature(x = 'dbMatrix', i = 'dbIndex', j = 'missing'),
  function(x, i, ..., drop = FALSE) {
            # get dbMatrix info
            con = get_con(x)
            dim = dim(x)

            # check inputs
            .check_extract(x = x, i = i, j = NULL, dim = dim)

            # create mapping of filtered rownames to row index
            map = data.frame(i = seq_along(rownames(x)), rowname = rownames(x))
            filter_i = get_dbM_sub_idx(index = i,
                                       dbM_dimnames = x@dim_names, dims = 1)
            map = map |>
              dplyr::filter(rowname %in% filter_i) |>
              dplyr::mutate(new_i = seq_along(filter_i)) # reset index

            # send map to db for subsetting
    name = unique_table_name('tmp_i')

            # FIXME: workaround for lack of support for writing
            # tables to a custom schema that is invisible to the user
            # i.e. not present in DBI::dbListTables(con)
            #
            # To see the tables in the arrow schema,
            # use duckdb::duckdb_list_arrow(conn = con)
            map_temp <- arrow::to_duckdb(
              .data = map, # converts to arrow-compliant object
              con = con,
              table_name = name,
              auto_disconnect = TRUE # remove tbl when gc
            )

            # subset dbMatrix
            x[] <- x[] |>
              dplyr::filter(i %in% !!map$i) |>
              dplyr::inner_join(map_temp, by = c("i" = "i")) |>
              dplyr::select(i = new_i, j, x)

            # update dbMatrix attributes
            x@dim_names[[1L]] <- filter_i
            x@dims[1L] <- length(filter_i)
            x@name <- NA_character_

            return(x)
  }
)

### cols only ####
#' @noRd
#' @concept dbMatrix
#' @export
setMethod(
  '[',
  signature(x = 'dbMatrix', i = 'missing',j = 'dbIndex'),
  function(x, j, ..., drop = FALSE) {
            # get dbMatrix info
            con <- get_con(x)
            dim = dim(x)

            # check for dims
            .check_extract(x = x, i = NULL, j = j, dim = dim)

            # create mapping of filtered colnames to col index
            map <- data.frame(j = seq_along(colnames(x)), colname = colnames(x))
            filter_j <- get_dbM_sub_idx(index = j,
                                       dbM_dimnames = x@dim_names, dims = 2)

            map <- map |>
              dplyr::filter(colname %in% filter_j) |>
              dplyr::mutate(new_j = seq_along(filter_j)) # reset index

            # send map to db for subsetting
    name <- unique_table_name('tmp_j')

            # FIXME:
            map_temp <- arrow::to_duckdb(
              .data = map, # converts to arrow-compliant object
              con = con,
              table_name = name,
              auto_disconnect = TRUE # remove tbl when gc
            )

            # Subset with arrow virtual table
            x[] <- x[] |>
              dplyr::filter(j %in% !!map$j) |>
              dplyr::inner_join(map_temp, by = c("j" = "j")) |>
              dplyr::select(i, j = new_j, x)

            # Update dbMatrix attributes
            x@dim_names[[2L]] <- filter_j
            x@dims[2L] <- length(filter_j)
            x@name <- NA_character_

            return(x)
  }
)

### rows and cols ####
#' @noRd
#' @concept dbMatrix
#' @export
setMethod('[',
          signature(x = 'dbMatrix', i = 'dbIndex', j = 'dbIndex'),
          function(x, i, j, ..., drop = FALSE) {
            # get dbMatrix info
            con = get_con(x)
            dim = dim(x)

            # check for dims
            .check_extract(x = x, i = i, j = j, dim = dim)

            # create mapping of dim indices and dimnames
            map_i = data.frame(i = seq_along(rownames(x)),
                               rowname = rownames(x))

            map_j = data.frame(j = seq_along(colnames(x)),
                               colname = colnames(x))

            # subset map by filtered dimnames
            filter_i = get_dbM_sub_idx(index = i,
                                       dbM_dimnames = x@dim_names, dims = 1)
            filter_j = get_dbM_sub_idx(index = j,
                                       dbM_dimnames = x@dim_names, dims = 2)

            map_i = map_i |>
              dplyr::filter(rowname %in% filter_i) |>
              dplyr::mutate(new_i = seq_along(filter_i)) # reset index

            map_j = map_j |>
              dplyr::filter(colname %in% filter_j) |>
              dplyr::mutate(new_j = seq_along(filter_j)) # reset index

            name = unique_table_name('tmp_map_ij_i')
            map_temp_j <- arrow::to_duckdb(
              .data = map_j, # converts to arrow-compliant object
              con = con,
              table_name = name,
              auto_disconnect = TRUE # remove tbl when gc
            )

            name = unique_table_name('tmp_map_ij_j')
            map_temp_i <- arrow::to_duckdb(
              .data = map_i, # converts to arrow-compliant object
              con = con,
              table_name = name,
              auto_disconnect = TRUE # remove tbl when gc
            )

            x[] <- x[] |>
              dplyr::filter(i %in% !!map_i$i, j %in% !!map_j$j) |>
              dplyr::inner_join(map_temp_i, by = c("i" = "i")) |>
              dplyr::inner_join(map_temp_j, by = c("j" = "j")) |>
              dplyr::select(i = new_i, j = new_j, x)

            # update dbMatrix attributes
            x@dim_names[[1L]] = filter_i
            x@dim_names[[2L]] = filter_j
            x@dims[1L] <- length(filter_i)
            x@dims[2L] <- length(filter_j)
            x@name <- NA_character_

            return(x)
          })
            
#' @description
#' Internal function to index `dbMatrix` objects by `dbIndex` superclass.
#' Can apply to both rows (dims = 1) and columns (dims = 2)
#' @keywords internal
#' @noRd
get_dbM_sub_idx = function(index, dbM_dimnames, dims) {
  # check that idx is 1 or 2
  if(dims != 1 && dims != 2){
    stop("dims must be 1 (rows) or 2 (columns)")
  }
  dims = as.integer(dims)

  if(is.character(index)){
    return(index)
  }

  if (is.logical(index)) {
    index <- recycle_boolean_index(index, length(dbM_dimnames[[dims]]))
  }

  if(is(index, 'dbDenseMatrix')){
    dimensions <- dim(index)

    # check that dimensions has 1 in the [1] or [2] position
    if (sum(dimensions == 1) != 1){
      stop("dbDenseMatrix is not a dbVector")
    }

    # FIXME: If dbmatrix@x is logical support recycle_boolean_index
    # below only supports character indexing, no recycling
    is_logical <- index@value |> head(1) |> dplyr::pull(x) |> is.logical()
    sub_names = dbM_dimnames[[dims]]
    if(is_logical){
      filtered_index <- index[] |>
        dplyr::filter(x == TRUE)
      if(dims == 1L || is(index, "dbDenseMatrix")){ #FIXME dbVector
        # a_rownames |>
        #    dplyr::semi_join(filtered_index, by = "i")
        index <- filtered_index |>
          dplyr::arrange(i) |>
          dplyr::distinct(i) |>
          dplyr::pull(i)
        return(sub_names[index])
      } else {
        index <- filtered_index |>
          dplyr::arrange(j) |>
          dplyr::distinct(j) |>
          dplyr::pull(j)
        return(sub_names[index])
      }
    }

    if(dimensions[1] <= length(dbM_dimnames[[dims]])){
      index <- rownames(index)
    } else if (dimensions[2] <= length(dbM_dimnames[[dims]])){
      index <- colnames(index)
    }

    if (all(index %in% dbM_dimnames[[dims]])){
      return(index)
    } else {
      stop("dbVector dimensions do not match dbMatrix dimensions")
    }
  }

  sub_names = dbM_dimnames[[dims]]
  return(sub_names[index])
}

#' @noRd
#' @keywords internal
recycle_boolean_index <- function(index, length) { #FIXME: dbVector
  if (is.logical(index) && length(index) < length) {
    recycled <- rep_len(index, length)
    return(which(recycled))
  }
  return(index)
}

#' @noRd
#' @keywords internal
.check_extract <- function(x = x, i = NULL, j = NULL, dim){
  if (!is.null(j)) {
    if ((is.numeric(j) || is.logical(j)) && max(j) > dim[2]) {
      stopf("Index exceeds column dimension of", dim[2])
    } else if (is.character(j) && !all(j %in% colnames(x))) {
      missing_cols <- j[!j %in% colnames(x)]
      stopf("Column(s) not found in dbMatrix: \n", missing_cols)
    }
  }

  if (!is.null(i)) {
    if ((is.numeric(i) || is.logical(i)) && max(i) > dim[1]) {
      stopf("Index exceeds row dimension of", dim[1])
    } else if (is.character(i) && !all(i %in% rownames(x))) {
      missing_rows <- i[!i %in% rownames(x)]
      stopf("Row(s) not found in dbMatrix: \n", missing_rows)
    }
  }
}

## matrix index ####
#' @noRd
#' @concept dbMatrix
#' @export
setMethod('[',
          signature(x = 'dbMatrix', i = 'dbMatrix', j = 'missing'),
          function(x, i, ..., drop = FALSE) {
            # Check dimensions match
            if (!identical(dim(x), dim(i))) {
              stop("Matrix index 'i' must have the same dimensions as 'x'")
            }

            # Perform join and filter
            res_tbl <- dplyr::inner_join(
              x[], i[], by = c("i", "j"), suffix = c(".x", ".i")
            ) |>
              dplyr::filter(x.i == TRUE) |>
              dplyr::arrange(j, i)

            # Pull the result into memory as a vector
            result_vector <- res_tbl |>
              dplyr::pull(x.x)

            # Note: Base R doesn't preserve names in this type of subsetting
            return(result_vector)
          })
            
            
# *** matrix index assignment ####
#' @noRd
#' @concept dbMatrix
#' @export
setMethod('[<-',
          signature(x = 'dbMatrix', i = 'dbMatrix', j = 'missing', value = 'ANY'),
          function(x, i, j, value) {
            if (!identical(dim(x), dim(i))) {
              stop("Logical index matrix 'i' must have the same dimensions as 'x'")
            }

            # Ensure value is scalar for this implementation
            if (length(value) != 1) {
              stop("Replacement value must be a single scalar for matrix indexing assignment")
            }

            con <- get_con(x)
            tbl_x <- x[]
            tbl_i <- i[]

            # Identify rows in tbl_x to update based on tbl_i
            rows_to_update <- dplyr::inner_join(
              tbl_x |> dplyr::select(i, j),
              tbl_i,
              by = c("i", "j")
            ) |>
              dplyr::filter(x == TRUE) |>
              dplyr::select(i, j)

            update_data <- rows_to_update |>
              dplyr::mutate(new_value = value)

            # Use rows_update for efficient update based on keys (i, j)
            update_data <- update_data |> dplyr::rename(x = new_value)

            # Perform the update
            updated_tbl <- dplyr::rows_update(
              tbl_x,
              update_data,
              by = c("i", "j"),
              unmatched = "ignore" # Keep rows from x that weren't updated
            )

            # Update the dbMatrix object's value slot
            x@value <- updated_tbl
            x@name <- NA_character_

            return(x)
          })

# dbDenseMatrix indexing methods ####
# These methods enable dbVector logical indexing for dbMatrix objects
# Critical for filterGiotto functionality in giottodb

#' @noRd
#' @concept dbMatrix
#' @export
setMethod(
  '[',
  signature(x = 'dbMatrix', i = 'dbDenseMatrix', j = 'missing'),
  function(x, i, ..., drop = FALSE) {
    # Check if i is a valid dbVector (1D matrix)
    if (!1 %in% dim(i)) {
      stopf("dbDenseMatrix index must be a dbVector (have 1 in dimensions)")
    }
    
    # Check dimensional compatibility - only row indexing supported
    if (dim(i)[1] != nrow(x)) {
      stopf("Row indexing: dbDenseMatrix must have same number of rows as matrix")
    }
    
    # Convert dbDenseMatrix to logical vector for indexing
    logical_index <- i[] |>
      dplyr::arrange(i) |>
      dplyr::pull(x) |>
      as.logical()
    
    # Use the existing dbIndex row indexing method
    return(x[logical_index, , drop = drop])
  }
)

#' @noRd
#' @concept dbMatrix  
#' @export
setMethod(
  '[',
  signature(x = 'dbMatrix', i = 'missing', j = 'dbDenseMatrix'),
  function(x, i, j, ..., drop = FALSE) {
    # Column indexing with dbDenseMatrix not supported due to dimensional constraints
    stopf("Column indexing with dbDenseMatrix not supported. Use as.vector(j) to convert to logical vector.")
  }
)

#' @noRd
#' @concept dbMatrix
#' @export  
setMethod(
  '[',
  signature(x = 'dbMatrix', i = 'dbDenseMatrix', j = 'dbDenseMatrix'),
  function(x, i, j, ..., drop = FALSE) {
    # Dual indexing with dbDenseMatrix not supported due to dimensional constraints
    stopf("Dual indexing with dbDenseMatrix not supported. Convert indices using as.vector() first.")
  }
)