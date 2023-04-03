
# Extract [] ####

setMethod('[', signature(x = 'dbMatrix', i = 'missing', j = 'missing', drop = 'missing'),
          function(x, i, j) {
            x@connection
          })

# setMethod('[[', signature(x = 'dbMatrix', i = 'missing', j = 'missing'),
#           function(x, i, j) {
#             dplyr::tbl(x@connection)
#           })













# Set [] ####

setMethod('[<-', signature(x = 'dbMatrix', i = 'missing', j = 'missing', value = 'character'),
          function(x, i, j, value) {
            dbpath = path.expand(value)
            if(!file.exists(dbpath)) {
              dbdir = gsub(basename(dbpath), '/', dbpath)
              dir.create(dbdir, recursive = TRUE)
            }

            con = duckdb::dbConnect(duckdb::duckdb(), dbdir = dbpath) # save to disk
            x@connection = con
            x@path = dbpath
            x
          })

setMethod('[<-', signature(x = 'dbMatrix', i = 'missing', j = 'missing', value = 'duckdb_connection'),
          function(x, i, j, value) {
            x@connection = value
            x@path = duckdb::dbGetInfo(x@connection)$dbname
            x
          })

setMethod('[<-', signature(x = 'dbMatrix', i = 'missing', j = 'missing', value = 'ANY'),
          function(x, i, j, value) {
            x@connection = value
            x
          })
