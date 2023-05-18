
# Functions specific to Giotto workflow

#' @name giotto_libNorm_dbMatrix
#' @title Giotto dbMatrix library normalization
#' @description
#' Workaround for SQL faster PARTITION aggregation via a
#' GROUP BY and INNER JOIN workflow for dbMatrix. Compatible with dplyr chains
#' @param dbMatrix dbMatrix to use
#' @param scalefactor scalefactor to use
#' @export
giotto_libNorm_dbMatrix = function(dbMatrix, scalefactor) {
  stopifnot(is.numeric(scalefactor))
  stopifnot(length(scalefactor) == 1L)
  p = dbMatrix[]$src$con
  conn = pool::poolCheckout(p)
  on.exit(pool::poolReturn(conn))

  quoted_scalefactor = DBI::dbQuoteLiteral(p, scalefactor)
  sql_query = paste0(
    'SELECT a.i, a.j, a.x/b.sum_x * ', quoted_scalefactor,' AS x
    FROM (
      SELECT *
      FROM (\n',
    dbplyr::sql_render(con = conn, dbMatrix[]),
    '\n)
    ) a
    INNER JOIN (
      SELECT j, SUM(x) AS sum_x
      FROM (\n',
    dbplyr::sql_render(con = conn, dbMatrix[]),
    '\n) GROUP BY j
    ) b
    ON a.j = b.j'
  )

  dbMatrix[] = dplyr::tbl(src = p, dbplyr::sql(sql_query))
  return(dbMatrix)
}













