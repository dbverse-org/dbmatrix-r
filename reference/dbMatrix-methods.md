# 

### dbDisconnect

\#' @title dbDisconnect \#' @rdname DBI \#' @export
setMethod('dbDisconnect', signature(x = 'dbMatrix'), function(x, ...)
con \<- get_con(x) DBI::dbDisconnect(conn = con, shutdown = TRUE) )

### dbListTables

\#' @title dbListTables \#' @rdname DBI \#' @export
setMethod('dbListTables', signature(x = 'dbMatrix'), function(x, ...)
con \<- get_con(x) DBI::dbListTables(conn = con) ) Create a dbMatrix
object computed in a database

## Usage

``` r
# S4 method for class 'DBIConnection'
dbLoad(conn, name, class)

# S4 method for class 'dbMatrix'
writeMM(obj, file, ...)
```

## Arguments

- conn:

  DBIConnection object

- name:

  valid name value (character)

- class:

  character, class of the dbMatrix object (e.g. "dbDenseMatrix" or
  "dbSparseMatrix")

- obj:

  dbMatrix object

- file:

  path to file

- ...:

  additional arguments
