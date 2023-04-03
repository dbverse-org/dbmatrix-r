
# Extract [] ####

setMethod('[', signature(x = 'dbData', i = 'missing', j = 'missing', drop = 'missing'),
          function(x, i, j) {
            x@connection
          })















# Set [] ####

setMethod('[<-', signature(x = 'dbData', i = 'missing', j = 'missing', value = 'duckdb_connection'),
          function(x, i, j, value) {
            x@connection = value
            x@path = duckdb::dbGetInfo(x@connection)$dbname
            x
          })

setMethod('[<-', signature(x = 'dbData', i = 'missing', j = 'missing', value = 'ANY'),
          function(x, i, j, value) {
            x@connection = value
            x
          })
