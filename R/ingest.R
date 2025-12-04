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

  ijx <- as_ijx(x)

  dplyr::copy_to(
    dest = con,
    name = name,
    df = ijx,
    overwrite = overwrite,
    ...
  )

  new(
    "dbSparseMatrix",
    value = dplyr::tbl(con, name),
    name = name,
    dims = dim(x),
    dim_names = dimnames(x),
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

  ijx <- data.frame(i = x@i + 1L, j = x@j + 1L, x = x@x)

  dplyr::copy_to(
    dest = con,
    name = name,
    df = ijx,
    overwrite = overwrite,
    ...
  )

  new(
    "dbSparseMatrix",
    value = dplyr::tbl(con, name),
    name = name,
    dims = dim(x),
    dim_names = dimnames(x),
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
