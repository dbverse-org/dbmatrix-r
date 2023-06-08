

#' internal function taht allows the addition of a primary key line to the table
#' generation sql from sqlCreateTable
#' @name sql_create_table
#' @title Generate sql to create a table with a primary key
#' @param con A database connection
#' @param table table name
#' @param fields a named character vector or a data frame
#' @param pk character. Which columns to select as the
#' primary key
#' @param row.names Either TRUE, FALSE, NA or a string.\cr
#' If TRUE, always translate row names to a column called "row_names". If FALSE,
#' never translate row names. If NA, translate rownames only if they're a
#' character vector. \cr
#' A string is equivalent to TRUE, but allows you to override the default name.\cr
#' For backward compatibility, NULL is equivalent to FALSE.
#' @param temporary If TRUE, will generate a temporary table statement.
#' @param ... additional params to pass
sql_create_pk_table = function(con,
                               table,
                               fields,
                               pk,
                               # increment = TRUE,
                               row.names = NA,
                               temporary = FALSE,
                               ...) {
  con = evaluate_conn(con, 'pool')
  checkmate::assert_character(pk)
  base_statement = pool::sqlCreateTable(con = con,
                                        table = table,
                                        fields = fields,
                                        row.names = row.names,
                                        temporary = temporary,
                                        ...)
  base_statement = as.character(base_statement)
  pk_line = paste0(',\n  PRIMARY KEY (',
                   paste0(pool::dbQuoteIdentifier(con, pk),
                          collapse = ', '),
                   ')\n)\n')
  dbplyr::sql(gsub('\n)\n$', pk_line, base_statement))
}







# internal function to swap out fields instructions if custom input is provided
# as a named list through fields_custom. Escaping of fields through
# dbQuoteIdentifier() is not needed since values are ONLY swapped if a match is
# is discovered with already escaped field names from sqlCreateTable
sql_create_table_field_swap = function(statement,
                                       fields_custom) {
  checkmate::assert_character(fields_custom)
  checkmate::assert_character(names(fields_custom))

  statement = as.character(statement)
  for(field in names(fields_custom)) {
    statement = gsub(paste0('\n  ', field, '[^\n|^,]*'),
                     paste0('\n  ', field, ' ', fields_custom[[field]]),
                     statement)
  }
  return(dplyr::sql(statement))
}









#' @name createTableBE
#' @title Create a table in the database backend
#' @description
#' Create a table in the backend. Adds some additional functionality on top of
#' what is expected from \code{\link[DBI]{dbCreateTable}}. A primary key can be
#' be provided at table creation. Partial inputs to define the table fields are
#' also possible, where the default data types and schema will be generated based
#' on a passed in data.frame, but additional information provided as a named list
#' to \code{fields_custom} param will override the relevant generated default.
#' @param conn A database connection
#' @param name table name
#' @param fields_df data frame from which to determine default data types
#' @param fields_custom named character vector of columns and manually assigned data types
#' along with any other in-line constraints
#' @param pk character. Which column(s) to select as the primary key
#' @param row.names Either TRUE, FALSE, NA or a string.\cr
#' If TRUE, always translate row names to a column called "row_names". If FALSE,
#' never translate row names. If NA, translate rownames only if they're a
#' character vector. \cr
#' A string is equivalent to TRUE, but allows you to override the default name.\cr
#' For backward compatibility, NULL is equivalent to FALSE.
#' @param temporary If TRUE, will generate a temporary table statement.
#' @param ... additional params to pass
createTableBE = function(conn,
                         name,
                         fields_df,
                         fields_custom = NULL,
                         pk = NULL,
                         ...,
                         row.names = NULL,
                         temporary = FALSE) {
  conn = evaluate_conn(conn)

  if(is.null(pk)) {
    statement = pool::sqlCreateTable(
      con = conn,
      table = name,
      fields = fields_df,
      row.names = row.names,
      temporary = temporary,
      ...
    )
  } else {
    statement = sql_create_pk_table(con = conn,
                                    table = name,
                                    fields = fields_df,
                                    pk = pk,
                                    row.names = row.names,
                                    temporary = temporary,
                                    ...)
    if(!is.null(fields_custom)) {
      statement = sql_create_table_field_swap(statement = statement,
                                              fields_custom = fields_custom)
    }
  }
  return(pool::dbExecute(conn = conn, statement = statement))
}



# fields presets ####
#' Package presets for DB table fields that can be easily accessed

#' @name fields_preset
#' @title Table fields presets
#' @description
#' Fields presets to use with DBI::dbCreateTable() to set up remote tables and
#' their constraints
#' @export
fields_preset = list(
  dbMatrix_ijx = c(i = 'VARCHAR(100) NOT NULL',
                   j = 'VARCHAR(100) NOT NULL',
                   x = 'DOUBLE'),
  dbPoly_geom = c(geom = 'UINTEGER NOT NULL',
                  part = 'UINTEGER NOT NULL',
                  x = 'DOUBLE NOT NULL',
                  y = 'DOUBLE NOT NULL',
                  hole = 'UINTEGER NOT NULL')
)
