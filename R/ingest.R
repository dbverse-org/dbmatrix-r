#' @title Convert object to dbMatrix
#' @description
#' Generic function to convert in-memory objects to `dbMatrix` objects.
#' @param x Object to convert (e.g., matrix, dgCMatrix)
#' @param con DBI or duckdb connection object
#' @param name Table name to assign within database
#' @param overwrite Whether to overwrite if table already exists
#' @param ... Additional arguments passed to methods
#' @export
as.dbMatrix <- function(
  x,
  con = NULL,
  name = "dbMatrix",
  overwrite = FALSE,
  ...
) {
  UseMethod("as.dbMatrix")
}

#' @export
as.dbMatrix.matrix <- function(
  x,
  con = NULL,
  name = "dbMatrix",
  overwrite = FALSE,
  ...
) {
  if (is.null(con)) {
    con <- DBI::dbConnect(duckdb::duckdb())
  }
  .check_con(con)
  .check_name(name)
  .check_overwrite(
    conn = con,
    overwrite = overwrite,
    name = name,
    skip_value_check = TRUE
  )

  dims <- dim(x)
  dim_names <- dimnames(x)
  if (is.null(dim_names)) {
    dim_names <- list(NULL, NULL)
  }
  if (is.null(dim_names[[1]])) {
    dim_names[[1]] <- paste0("row", seq_len(dims[1]))
  }
  if (is.null(dim_names[[2]])) {
    dim_names[[2]] <- colnames(x) %||% paste0("col", seq_len(dims[2]))
  }

  # Override column names to avoid registration issues with duplicates
  colnames(x) <- paste0("c", seq_len(ncol(x)))

  tmp_view <- unique_table_name("tmp_view_dense")
  duckdb::duckdb_register(con, tmp_view, x)
  on.exit(
    DBI::dbExecute(con, glue::glue("DROP VIEW IF EXISTS {tmp_view}")),
    add = TRUE
  )

  # UNPIVOT and map columns using pragma_table_info
  # pragma_table_info('{tmp_view}') provides metadata about the view's columns,
  # including the column name and its 0-based index (cid).
  # By joining the unpivoted column name (j_name) with this metadata, we can
  # efficiently map columns to their integer index 'j' entirely within DuckDB,
  # avoiding the need to create and upload a separate mapping table from R.
  sql <- glue::glue(
    "
    CREATE TABLE {name} AS
    WITH unpivoted AS (
      SELECT row_number() OVER () as i, * FROM {tmp_view}
    )
    SELECT 
      u.i, 
      (m.cid + 1) as j, 
      u.x 
    FROM (
      SELECT * FROM unpivoted
      UNPIVOT INCLUDE NULLS (
        x FOR j_name IN (* EXCLUDE (i))
      )
    ) u
    JOIN pragma_table_info('{tmp_view}') m ON u.j_name = m.name
  "
  )

  DBI::dbExecute(con, sql)

  new(
    "dbDenseMatrix",
    value = dplyr::tbl(con, name),
    name = name,
    dims = dims,
    dim_names = dim_names,
    init = TRUE
  )
}

#' @export
as.dbMatrix.dgCMatrix <- function(
  x,
  con = NULL,
  name = "dbMatrix",
  overwrite = FALSE,
  ...
) {
  if (is.null(con)) {
    con <- DBI::dbConnect(duckdb::duckdb())
  }
  .check_con(con)
  .check_name(name)
  .check_overwrite(
    conn = con,
    overwrite = overwrite,
    name = name,
    skip_value_check = TRUE
  )

  # Extract directly from dgCMatrix format
  # We need to construct 1-based (i, j, x) triplets for dbMatrix
  n_nz <- length(x@x)
  ncol_mat <- ncol(x)
  
  # Column indices: for each column j (1-based), repeat j for each entry in that column
  # diff(@p) gives count of entries per column
  j_idx <- rep.int(seq_len(ncol_mat), diff(x@p))
  
  # Row indices: @i is 0-based, convert to 1-based
  i_idx <- x@i + 1L
  
  arrow_tbl <- arrow::arrow_table(
    i = i_idx,
    j = j_idx,
    x = x@x
  )
  
  temp_arrow_name <- unique_table_name("__dbM_arrow_ingest")
  
  duckdb::duckdb_register_arrow(con, temp_arrow_name, arrow_tbl)
  
  tryCatch({
    if (overwrite) {
      DBI::dbExecute(con, glue::glue("DROP TABLE IF EXISTS \"{name}\""))
    }
    DBI::dbExecute(con, glue::glue("CREATE TABLE \"{name}\" AS SELECT * FROM \"{temp_arrow_name}\""))
  }, finally = {
    duckdb::duckdb_unregister_arrow(con, temp_arrow_name)
  })

  new(
    "dbSparseMatrix",
    value = dplyr::tbl(con, name),
    name = name,
    dims = dim(x),
    dim_names = .ensure_dim_names(dimnames(x), dim(x)),
    init = TRUE
  )
}

#' @export
as.dbMatrix.dgTMatrix <- function(
  x,
  con = NULL,
  name = "dbMatrix",
  overwrite = FALSE,
  ...
) {
  if (is.null(con)) {
    con <- DBI::dbConnect(duckdb::duckdb())
  }
  .check_con(con)
  .check_name(name)
  .check_overwrite(
    conn = con,
    overwrite = overwrite,
    name = name,
    skip_value_check = TRUE
  )

  # dgTMatrix slots @i and @j are 0-based, convert to 1-based
  arrow_tbl <- arrow::arrow_table(
    i = x@i + 1L,
    j = x@j + 1L,
    x = x@x
  )
  
  temp_arrow_name <- unique_table_name("__dbM_arrow_ingest")
  
  duckdb::duckdb_register_arrow(con, temp_arrow_name, arrow_tbl)
  
  tryCatch({
    if (overwrite) {
      DBI::dbExecute(con, glue::glue("DROP TABLE IF EXISTS \"{name}\""))
    }
    DBI::dbExecute(con, glue::glue("CREATE TABLE \"{name}\" AS SELECT * FROM \"{temp_arrow_name}\""))
  }, finally = {
    duckdb::duckdb_unregister_arrow(con, temp_arrow_name)
  })

  new(
    "dbSparseMatrix",
    value = dplyr::tbl(con, name),
    name = name,
    dims = dim(x),
    dim_names = .ensure_dim_names(dimnames(x), dim(x)),
    init = TRUE
  )
}

#' @export
as.dbMatrix.Matrix <- function(
  x,
  con = NULL,
  name = "dbMatrix",
  overwrite = FALSE,
  ...
) {
  # Supported only whitelisted Matrix classes
  # dMatrix: Double (numeric)
  # iMatrix: Integer
  # lMatrix: Logical (coerced to numeric 1/0)
  # nMatrix: Pattern (coerced to numeric 1/0)
  if (
    !inherits(x, "dMatrix") &&
      !inherits(x, "iMatrix") &&
      !inherits(x, "lMatrix") &&
      !inherits(x, "nMatrix")
  ) {
    stop(
      "Only numeric, integer, logical, and pattern matrices are supported. Input class: ",
      class(x)[1],
      call. = FALSE
    )
  }

  if (inherits(x, "sparseMatrix")) {
    # Some classes (like dgRMatrix) have no direct coercion to dgCMatrix
    # but can be coerced to CsparseMatrix first.
    if (!inherits(x, "dgCMatrix")) {
      x <- as(x, "CsparseMatrix")
      if (!inherits(x, "dgCMatrix")) {
        x <- as(x, "dgCMatrix")
      }
    }
    as.dbMatrix.dgCMatrix(x, con, name, overwrite, ...)
  } else {
    as.dbMatrix.matrix(as.matrix(x), con, name, overwrite, ...)
  }
}

#' @export
as.dbMatrix.default <- function(
  x,
  con = NULL,
  name = "dbMatrix",
  overwrite = FALSE,
  ...
) {
  stop(
    "Unsupported object class: ",
    class(x)[1],
    ". Input must be a matrix or Matrix object.",
    call. = FALSE
  )
}
