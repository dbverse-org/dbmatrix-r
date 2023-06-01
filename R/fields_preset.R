





#' @name fields_preset
#' @title Table fields presets
#' @description
#' Fields presets to use with DBI::dbCreateTable() to set up remote tables and
#' their constraints
#' @export
fields_preset = list(
  dbMatrix_ijx = c(i = 'VARCHAR(100) NOT NULL',
                   j = 'VARCHAR(100) NOT NULL',
                   x = 'DOUBLE')
)
