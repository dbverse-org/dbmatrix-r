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
    con <- get_con(x)
    dim <- dim(x)

    # check inputs
    .check_extract(x = x, i = i, j = NULL, dim = dim)

    # Convert character indices to integers for faster extraction
    if (is.character(i)) {
      int_idx <- match(i, x@dim_names[[1]])
      if (anyNA(int_idx)) {
        bad_names <- i[is.na(int_idx)]
        stop("Invalid row names: ", paste(head(bad_names, 5), collapse = ", "),
             if (length(bad_names) > 5) paste0(", ... (", length(bad_names), " total)"))
      }
      i <- int_idx
    }

    n_i <- NA_integer_

    if (is.numeric(i)) {
      # Handle negative indices: convert to positive by excluding
      if (any(i < 0)) {
        if (any(i > 0)) {
          stop("Cannot mix positive and negative indices")
        }
        i <- seq_len(dim[1])[i]  # Convert negative indices to positive
      }
      
      if (is.logical(i)) {
        if (length(i) < dim[1]) {
          i <- rep_len(i, dim[1])
        }
        i <- which(i)
      }

      i_idx <- as.integer(i)
      n_i <- length(i_idx)

      # Identity fast path: full set of rows in original order
      if (length(i_idx) == dim[1] && identical(i_idx, seq_len(dim[1]))) {
        return(x)
      }

      # <2000 use inline SQL, >=2000 use register (avoids massive SQL strings)
      if (length(i_idx) < 2000) {
        values_list <- glue::glue_collapse(
          glue::glue("({seq_along(i_idx)}, {i_idx})"),
          sep = ", "
        )
        sql <- glue::glue(
          "SELECT * FROM (VALUES {values_list}) AS map(new_i, i)"
        )
        map_tbl <- dplyr::tbl(con, dplyr::sql(sql))
      } else {
        req_df <- data.frame(
          new_i = seq_along(i_idx),
          i = i_idx,
          stringsAsFactors = FALSE
        )
        req_tbl_name <- unique_table_name("__dbM_extract_req_i")
        dplyr::copy_to(
          dest = con,
          req_df,
          name = req_tbl_name,
          temporary = TRUE,
          overwrite = TRUE
        )
        map_tbl <- dplyr::tbl(con, req_tbl_name)
      }
      # Handle NULL dim_names: keep NULL or subset existing names
      filter_i <- x@dim_names[[1]][i_idx]  # Returns NULL if dim_names[[1]] is NULL
    } else {
      filter_i <- get_dbM_sub_idx(
        index = i,
        dbM_dimnames = x@dim_names,
        dims = 1
      )

      n_i <- length(filter_i)

      # Identity fast path: if this selection is the full set of rows in the
      # original order, subsetting is a no-op and we can avoid building
      # mapping tables and joins.
      orig_i <- x@dim_names[[1]]
      if (!is.null(orig_i)) {
        if (length(filter_i) == length(orig_i) &&
            identical(as.character(filter_i), as.character(orig_i))) {
          return(x)
        }
      }

      # <2000 use inline SQL, >=2000 use register (avoids massive SQL strings)
      if (length(filter_i) < 2000) {
        safe_names <- gsub("'", "''", filter_i, fixed = TRUE)
        values_list <- glue::glue_collapse(
          glue::glue("({seq_along(filter_i)}, '{safe_names}')"),
          sep = ", "
        )
        sql <- glue::glue(
          "SELECT * FROM (VALUES {values_list}) AS map(new_i, rowname)"
        )
        req_tbl <- dplyr::tbl(con, dplyr::sql(sql))
      } else {
        req_df <- data.frame(
          new_i = seq_along(filter_i),
          rowname = as.character(filter_i),
          stringsAsFactors = FALSE
        )
        req_tbl_name <- unique_table_name("__dbM_extract_req_i")
        dplyr::copy_to(
          dest = con,
          req_df,
          name = req_tbl_name,
          temporary = TRUE,
          overwrite = TRUE
        )
        req_tbl <- dplyr::tbl(con, req_tbl_name)
      }

      # Use helper to create dimension mapping
      dim_map_tbl <- store_mapping(
        con = con, 
        items = as.character(x@dim_names[[1]]), 
        prefix = "__dbM_extract_dim_map_i", 
        col_name_in_db = "rowname"
      )

      map_tbl <- req_tbl |>
        dplyr::inner_join(
          dim_map_tbl,
          by = "rowname"
        ) |>
        dplyr::rename(i = idx) |>
        dplyr::select(new_i, i)
    }

    x_tbl <- x[]
    if (exists("i_idx") && length(i_idx) < 2000) {
      x_tbl <- dplyr::filter(x_tbl, i %in% i_idx)
    }
    x[] <- x_tbl |>
      dplyr::inner_join(map_tbl, by = "i") |>
      dplyr::select(i = new_i, j, x)

    # Preserve factor status for dim_names
    x@dim_names[[1L]] <- if (is.factor(filter_i)) filter_i else as.factor(filter_i)
    x@dims[1L] <- n_i
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
  signature(x = 'dbMatrix', i = 'missing', j = 'dbIndex'),
  function(x, j, ..., drop = FALSE) {
    con <- get_con(x)
    dim <- dim(x)

    # check for dims
    .check_extract(x = x, i = NULL, j = j, dim = dim)

    # Convert character indices to integers for faster extraction
    if (is.character(j)) {
      int_idx <- match(j, x@dim_names[[2]])
      if (anyNA(int_idx)) {
        bad_names <- j[is.na(int_idx)]
        stop("Invalid column names: ", paste(head(bad_names, 5), collapse = ", "),
             if (length(bad_names) > 5) paste0(", ... (", length(bad_names), " total)"))
      }
      j <- int_idx
    }

    n_j <- NA_integer_

    if (is.numeric(j)) {
      # Handle negative indices: convert to positive by excluding
      if (any(j < 0)) {
        if (any(j > 0)) {
          stop("Cannot mix positive and negative indices")
        }
        j <- seq_len(dim[2])[j]  # Convert negative indices to positive
      }
      
      if (is.logical(j)) {
        if (length(j) < dim[2]) {
          j <- rep_len(j, dim[2])
        }
        j <- which(j)
      }

      j_idx <- as.integer(j)
      n_j <- length(j_idx)

      # Identity fast path: full set of columns in original order
      if (length(j_idx) == dim[2] && identical(j_idx, seq_len(dim[2]))) {
        return(x)
      }

      # <2000 use inline SQL, >=2000 use register (avoids massive SQL strings)
      if (length(j_idx) < 2000) {
        values_list <- glue::glue_collapse(
          glue::glue("({seq_along(j_idx)}, {j_idx})"),
          sep = ", "
        )
        sql <- glue::glue(
          "SELECT * FROM (VALUES {values_list}) AS map(new_j, j)"
        )
        map_tbl <- dplyr::tbl(con, dplyr::sql(sql))
      } else {
        req_df <- data.frame(
          new_j = seq_along(j_idx),
          j = j_idx,
          stringsAsFactors = FALSE
        )
        req_tbl_name <- unique_table_name("__dbM_extract_req_j")
        dplyr::copy_to(
          dest = con,
          req_df,
          name = req_tbl_name,
          temporary = TRUE,
          overwrite = TRUE
        )
        map_tbl <- dplyr::tbl(con, req_tbl_name)
      }
      # Handle NULL dim_names: keep NULL or subset existing names
      filter_j <- x@dim_names[[2]][j_idx]  # Returns NULL if dim_names[[2]] is NULL
    } else {
      filter_j <- get_dbM_sub_idx(
        index = j,
        dbM_dimnames = x@dim_names,
        dims = 2
      )

      n_j <- length(filter_j)

      # Identity fast path: if this selection is the full set of columns in the
      # original order, subsetting is a no-op and we can avoid building
      # mapping tables and joins.
      orig_j <- x@dim_names[[2]]
      if (!is.null(orig_j)) {
        if (length(filter_j) == length(orig_j) &&
            identical(as.character(filter_j), as.character(orig_j))) {
          return(x)
        }
      }

      # <2000 use inline SQL, >=2000 use register (avoids massive SQL strings)
      if (length(filter_j) < 2000) {
        safe_names <- gsub("'", "''", filter_j, fixed = TRUE)
        values_list <- glue::glue_collapse(
          glue::glue("({seq_along(filter_j)}, '{safe_names}')"),
          sep = ", "
        )
        sql <- glue::glue(
          "SELECT * FROM (VALUES {values_list}) AS map(new_j, colname)"
        )
        req_tbl <- dplyr::tbl(con, dplyr::sql(sql))
      } else {
        req_df <- data.frame(
          new_j = seq_along(filter_j),
          colname = as.character(filter_j),
          stringsAsFactors = FALSE
        )
        req_tbl_name <- unique_table_name("__dbM_extract_req_j")
        dplyr::copy_to(
          dest = con,
          req_df,
          name = req_tbl_name,
          temporary = TRUE,
          overwrite = TRUE
        )
        req_tbl <- dplyr::tbl(con, req_tbl_name)
      }

      # Use helper to create dimension mapping
      dim_map_tbl <- store_mapping(
        con = con, 
        items = as.character(x@dim_names[[2]]), 
        prefix = "__dbM_extract_dim_map_j", 
        col_name_in_db = "colname"
      )

      map_tbl <- req_tbl |>
        dplyr::inner_join(
          dim_map_tbl,
          by = "colname"
        ) |>
        dplyr::rename(j = idx) |>
        dplyr::select(new_j, j)
    }

    x_tbl <- x[]
    if (exists("j_idx") && length(j_idx) < 2000) {
      x_tbl <- dplyr::filter(x_tbl, j %in% j_idx)
    }
    x[] <- x_tbl |>
      dplyr::inner_join(map_tbl, by = "j") |>
      dplyr::select(i, j = new_j, x)

    # Preserve factor status for dim_names
    x@dim_names[[2L]] <- if (is.factor(filter_j)) filter_j else as.factor(filter_j)
    x@dims[2L] <- n_j
    x@name <- NA_character_

    return(x)
  }
)

### rows and cols ####
#' @noRd
#' @concept dbMatrix
#' @export
setMethod(
  '[',
  signature(x = 'dbMatrix', i = 'dbIndex', j = 'dbIndex'),
  function(x, i, j, ..., drop = FALSE) {
    # get dbMatrix info
    con <- get_con(x)
    dim <- dim(x)

    # check for dims
    .check_extract(x = x, i = i, j = j, dim = dim)

    # Convert character indices to integers for faster extraction
    if (is.character(i)) {
      int_idx <- match(i, x@dim_names[[1]])
      if (anyNA(int_idx)) {
        bad_names <- i[is.na(int_idx)]
        stop("Invalid row names: ", paste(head(bad_names, 5), collapse = ", "),
             if (length(bad_names) > 5) paste0(", ... (", length(bad_names), " total)"))
      }
      i <- int_idx
    }
    
    if (is.character(j)) {
      int_idx <- match(j, x@dim_names[[2]])
      if (anyNA(int_idx)) {
        bad_names <- j[is.na(int_idx)]
        stop("Invalid column names: ", paste(head(bad_names, 5), collapse = ", "),
             if (length(bad_names) > 5) paste0(", ... (", length(bad_names), " total)"))
      }
      j <- int_idx
    }

    # Process i index (same logic as row-only subsetting)
    if (is.numeric(i)) {
      # Handle negative indices: convert to positive by excluding
      if (any(i < 0)) {
        if (any(i > 0)) {
          stop("Cannot mix positive and negative indices")
        }
        i <- seq_len(dim[1])[i]  # Convert negative indices to positive
      }
      
      if (is.logical(i)) {
        if (length(i) < dim[1]) {
          i <- rep_len(i, dim[1])
        }
        i <- which(i)
      }

      # <2000 use inline SQL, >=2000 use register (avoids massive SQL strings)
      if (length(i) < 2000) {
        values_list <- glue::glue_collapse(
          glue::glue("({seq_along(i)}, {as.integer(i)})"),
          sep = ", "
        )
        sql <- glue::glue(
          "SELECT * FROM (VALUES {values_list}) AS map(new_i, i)"
        )
        map_tbl_i <- dplyr::tbl(con, dplyr::sql(sql))
      } else {
        req_df <- data.frame(
          new_i = seq_along(i),
          i = as.integer(i),
          stringsAsFactors = FALSE
        )
        req_tbl_name <- unique_table_name("__dbM_extract_req_i")
        dplyr::copy_to(
          dest = con,
          req_df,
          name = req_tbl_name,
          temporary = TRUE,
          overwrite = TRUE
        )
        map_tbl_i <- dplyr::tbl(con, req_tbl_name)
      }
      # Handle NULL dim_names: keep NULL or subset existing names
      filter_i <- x@dim_names[[1]][i]  # Returns NULL if dim_names[[1]] is NULL
    } else {
      filter_i <- get_dbM_sub_idx(
        index = i,
        dbM_dimnames = x@dim_names,
        dims = 1
      )

      # <2000 use inline SQL, >=2000 use register (avoids massive SQL strings)
      if (length(filter_i) < 2000) {
        safe_names <- gsub("'", "''", filter_i, fixed = TRUE)
        values_list <- glue::glue_collapse(
          glue::glue("({seq_along(filter_i)}, '{safe_names}')"),
          sep = ", "
        )
        sql <- glue::glue(
          "SELECT * FROM (VALUES {values_list}) AS map(new_i, rowname)"
        )
        req_tbl <- dplyr::tbl(con, dplyr::sql(sql))
      } else {
        req_df <- data.frame(
          new_i = seq_along(filter_i),
          rowname = as.character(filter_i),
          stringsAsFactors = FALSE
        )
        req_tbl_name <- unique_table_name("__dbM_extract_req_i")
        dplyr::copy_to(
          dest = con,
          req_df,
          name = req_tbl_name,
          temporary = TRUE,
          overwrite = TRUE
        )
        req_tbl <- dplyr::tbl(con, req_tbl_name)
      }

      # Use helper to create dimension mapping
      dim_map_tbl <- store_mapping(
        con = con, 
        items = as.character(x@dim_names[[1]]), 
        prefix = "__dbM_extract_dim_map_i", 
        col_name_in_db = "rowname"
      )

      map_tbl_i <- req_tbl |>
        dplyr::inner_join(
          dim_map_tbl,
          by = "rowname"
        ) |>
        dplyr::rename(i = idx) |>
        dplyr::select(new_i, i)
    }

    # Process j index (same logic as column-only subsetting)
    if (is.numeric(j)) {
      # Handle negative indices: convert to positive by excluding
      if (any(j < 0)) {
        if (any(j > 0)) {
          stop("Cannot mix positive and negative indices")
        }
        j <- seq_len(dim[2])[j]  # Convert negative indices to positive
      }
      
      if (is.logical(j)) {
        if (length(j) < dim[2]) {
          j <- rep_len(j, dim[2])
        }
        j <- which(j)
      }

      # <2000 use inline SQL, >=2000 use register (avoids massive SQL strings)
      if (length(j) < 2000) {
        values_list <- glue::glue_collapse(
          glue::glue("({seq_along(j)}, {as.integer(j)})"),
          sep = ", "
        )
        sql <- glue::glue(
          "SELECT * FROM (VALUES {values_list}) AS map(new_j, j)"
        )
        map_tbl_j <- dplyr::tbl(con, dplyr::sql(sql))
      } else {
        req_df <- data.frame(
          new_j = seq_along(j),
          j = as.integer(j),
          stringsAsFactors = FALSE
        )
        req_tbl_name <- unique_table_name("__dbM_extract_req_j")
        dplyr::copy_to(
          dest = con,
          req_df,
          name = req_tbl_name,
          temporary = TRUE,
          overwrite = TRUE
        )
        map_tbl_j <- dplyr::tbl(con, req_tbl_name)
      }

      filter_j <- x@dim_names[[2]][j]
    } else {
      filter_j <- get_dbM_sub_idx(
        index = j,
        dbM_dimnames = x@dim_names,
        dims = 2
      )

      # <2000 use inline SQL, >=2000 use register (avoids massive SQL strings)
      if (length(filter_j) < 2000) {
        safe_names <- gsub("'", "''", filter_j, fixed = TRUE)
        values_list <- glue::glue_collapse(
          glue::glue("({seq_along(filter_j)}, '{safe_names}')"),
          sep = ", "
        )
        sql <- glue::glue(
          "SELECT * FROM (VALUES {values_list}) AS map(new_j, colname)"
        )
        req_tbl <- dplyr::tbl(con, dplyr::sql(sql))
      } else {
        req_df <- data.frame(
          new_j = seq_along(filter_j),
          colname = as.character(filter_j),
          stringsAsFactors = FALSE
        )
        req_tbl_name <- unique_table_name("__dbM_extract_req_j")
        dplyr::copy_to(
          dest = con,
          req_df,
          name = req_tbl_name,
          temporary = TRUE,
          overwrite = TRUE
        )
        req_tbl <- dplyr::tbl(con, req_tbl_name)
      }

      # Use helper to create dimension mapping
      dim_map_tbl <- store_mapping(
        con = con, 
        items = as.character(x@dim_names[[2]]), 
        prefix = "__dbM_extract_dim_map_j", 
        col_name_in_db = "colname"
      )

      map_tbl_j <- req_tbl |>
        dplyr::inner_join(
          dim_map_tbl,
          by = "colname"
        ) |>
        dplyr::rename(j = idx) |>
        dplyr::select(new_j, j)
    }

    x[] <- x[] |>
      dplyr::inner_join(map_tbl_i, by = "i") |>
      dplyr::inner_join(map_tbl_j, by = "j") |>
      dplyr::select(i = new_i, j = new_j, x)

    # update dbMatrix attributes - preserve factor status
    x@dim_names[[1L]] <- if (is.factor(filter_i)) filter_i else as.factor(filter_i)
    x@dim_names[[2L]] <- if (is.factor(filter_j)) filter_j else as.factor(filter_j)
    x@dims[1L] <- if (!is.null(filter_i)) length(filter_i) else length(i)
    x@dims[2L] <- if (!is.null(filter_j)) length(filter_j) else length(j)
    x@name <- NA_character_

    return(x)
  }
)

#' @description
#' Internal function to index `dbMatrix` objects by `dbIndex` superclass.
#' Can apply to both rows (dims = 1) and columns (dims = 2)
#' @keywords internal
#' @noRd
get_dbM_sub_idx <- function(index, dbM_dimnames, dims) {
  # check that idx is 1 or 2
  if (dims != 1 && dims != 2) {
    stop("dims must be 1 (rows) or 2 (columns)")
  }
  dims <- as.integer(dims)

  if (is.character(index)) {
    return(index)
  }

  if (is.logical(index)) {
    index <- recycle_boolean_index(index, length(dbM_dimnames[[dims]]))
  }

  if (is(index, 'dbDenseMatrix')) {
    dimensions <- dim(index)

    # check that dimensions has 1 in the [1] or [2] position
    if (sum(dimensions == 1) != 1) {
      stop("dbDenseMatrix is not a 1D dbMatrix")
    }

    # FIXME: If dbmatrix@x is logical support recycle_boolean_index
    # below only supports character indexing, no recycling
    is_logical <- index@value |> head(1) |> dplyr::pull(x) |> is.logical()
    sub_names <- dbM_dimnames[[dims]]
    if (is_logical) {
      filtered_index <- index[] |>
        dplyr::filter(x)
      if (dims == 1L || is(index, "dbDenseMatrix")) {
        #FIXME 1D dbMatrix
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

    if (dimensions[1] <= length(dbM_dimnames[[dims]])) {
      index <- rownames(index)
    } else if (dimensions[2] <= length(dbM_dimnames[[dims]])) {
      index <- colnames(index)
    }

    if (all(index %in% dbM_dimnames[[dims]])) {
      return(index)
    } else {
      stop("1D dbMatrix dimensions do not match dbMatrix dimensions")
    }
  }

  sub_names <- dbM_dimnames[[dims]]
  return(sub_names[index])
}

#' @noRd
#' @keywords internal
recycle_boolean_index <- function(index, length) {
  #FIXME: 1D dbMatrix
  if (is.logical(index) && length(index) < length) {
    recycled <- rep_len(index, length)
    return(which(recycled))
  }
  return(index)
}

#' @noRd
#' @keywords internal
.check_extract <- function(x = x, i = NULL, j = NULL, dim) {
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
setMethod(
  '[',
  signature(x = 'dbMatrix', i = 'dbMatrix', j = 'missing'),
  function(x, i, ..., drop = FALSE) {
    # Check dimensions match
    if (!identical(dim(x), dim(i))) {
      stop("Matrix index 'i' must have the same dimensions as 'x'")
    }

    # Perform join and filter
    res_tbl <- dplyr::inner_join(
      x[],
      i[],
      by = c("i", "j"),
      suffix = c(".x", ".i")
    ) |>
      dplyr::filter(x.i) |>
      dplyr::arrange(j, i)

    # Pull the result into memory as a vector
    result_vector <- res_tbl |>
      dplyr::pull(x.x)

    # Note: Base R doesn't preserve names in this type of subsetting
    return(result_vector)
  }
)


# *** matrix index assignment ####
#' @noRd
#' @concept dbMatrix
#' @export
setMethod(
  '[<-',
  signature(x = 'dbMatrix', i = 'dbMatrix', j = 'missing', value = 'ANY'),
  function(x, i, j, value) {
    if (!identical(dim(x), dim(i))) {
      stop("Logical index matrix 'i' must have the same dimensions as 'x'")
    }

    # Ensure value is scalar for this implementation
    if (length(value) != 1) {
      stop(
        "Replacement value must be a single scalar for matrix indexing assignment"
      )
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
      dplyr::filter(x) |>
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
  }
)

# dbDenseMatrix indexing methods ####
# These methods enable 1D dbMatrix logical indexing for dbMatrix objects

#' Internal helper to store mapping table for subsetting
#' Uses inline SQL for small N, registered virtual table (view) for large N
#' Fallback to registered view ensures efficiency and clean database state
#' @keywords internal
#' @noRd
store_mapping <- function(con, items, prefix, col_name_in_db) {
  # Inline SQL
  if (length(items) < 2000) {
    safe_items <- gsub("'", "''", items, fixed = TRUE)
    items_sql <- glue::glue_collapse(glue::glue("'{safe_items}'"), sep = ", ")
    
    mapping_sql <- glue::glue(
      "SELECT ROW_NUMBER() OVER () as idx, unnest as {col_name_in_db} 
       FROM (SELECT UNNEST([{items_sql}]) as unnest)"
    )
    return(dplyr::tbl(con, dplyr::sql(glue::glue("({mapping_sql})"))))
  } 
  
  # Registered virtual table
  df <- data.frame(
    idx = seq_along(items),
    name = items,
    stringsAsFactors = FALSE
  )
  colnames(df)[2] <- col_name_in_db
  
  tbl_name <- unique_table_name(prefix)
  dplyr::copy_to(
    dest = con,
    df,
    name = tbl_name,
    temporary = TRUE,
    overwrite = TRUE
  )
  return(dplyr::tbl(con, tbl_name))
}

#' @noRd
#' @concept dbMatrix
#' @export
setMethod(
  '[',
  signature(x = 'dbMatrix', i = 'dbDenseMatrix', j = 'missing'),
  function(x, i, ..., drop = FALSE) {
    # Check if i is a valid 1D dbMatrix (1D matrix)
    if (!1 %in% dim(i)) {
      stopf("dbDenseMatrix index must be a 1D dbMatrix (have 1 in dimensions)")
    }

    # Check dimensional compatibility - only row indexing supported
    if (dim(i)[1] != nrow(x)) {
      stopf(
        "Row indexing: dbDenseMatrix must have same number of rows as matrix"
      )
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
    stopf(
      "Column indexing with dbDenseMatrix not supported. Use as.vector(j) to convert to logical vector."
    )
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
    stopf(
      "Dual indexing with dbDenseMatrix not supported. Convert indices using as.vector() first."
    )
  }
)
