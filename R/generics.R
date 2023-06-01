

# dbData object interactions ####
setGeneric('cPool', function(x, ...) standardGeneric('cPool'))
setGeneric('cPool<-', function(x, ..., value) standardGeneric('cPool<-'))
setGeneric('remoteName', function(x, ...) standardGeneric('remoteName'))
setGeneric('reconnect', function(x, ...) standardGeneric('reconnect'))
setGeneric('disconnect', function(x, ...) standardGeneric('disconnect'))
setGeneric('queryStack', function(x, ...) standardGeneric('queryStack'))
setGeneric('queryStack<-', function(x, ..., value) standardGeneric('queryStack<-'))
setGeneric('sql_query', function(x, statement, ...) standardGeneric('sql_query'))
setGeneric('colTypes', function(x, ...) standardGeneric('colTypes'))
setGeneric('castNumeric', function(x, col, ...) standardGeneric('castNumeric'))
setGeneric('create_index', function(x, name, column, unique, ...) standardGeneric('create_index'))
setGeneric('is_init', function(x, ...) standardGeneric('is_init'))

# dbSpatProxyData object interactions ####
setGeneric('chunkSpatApply', function(x, y, fun, extent, ...) standardGeneric('chunkSpatApply'))
setGeneric('extent_calculate', function(x, ...) standardGeneric('extent_calculate'))
setGeneric('extent_filter', function(x, extent, include, ...) standardGeneric('extent_filter'))
setGeneric('filter_dbspat', function(x, by_geom, by_value, ...) standardGeneric('filter_dbspat'))
setGeneric('dbspat_to_sv', function(x, ...) standardGeneric('dbspat_to_sv'))

# backend system interactions ####
setGeneric('append')
setGeneric('validBE', function(x, ...) standardGeneric('validBE'))
setGeneric('tableInfo', function(x, remote_name, ...) standardGeneric('tableInfo'))
setGeneric('listTablesBE', function(x, ...) standardGeneric('listTablesBE'))
setGeneric('existsTableBE', function(x, remote_name, ...) standardGeneric('existsTableBE'))
setGeneric('backendID', function(x, ...) standardGeneric('backendID'))
setGeneric('backendID<-', function(x, ..., value) standardGeneric('backendID<-'))
setGeneric('reconnectBackend', function(x, ...) standardGeneric('reconnectBackend'))
setGeneric('dbms', function(x, ...) standardGeneric('dbms'))
setGeneric('evaluate_conn', function(conn, ...) standardGeneric('evaluate_conn'))
setGeneric('dbSettings', function(x, setting, value, ...) standardGeneric('dbSettings'))
# setGeneric('setRemoteKey', function(x, remote_name, primary_key, ...) standardGeneric('setRemoteKey'))

# dbMatrix specific ####
setGeneric('colSds', function(x, ...) standardGeneric('colSds'))
setGeneric('rowSds', function(x, ...) standardGeneric('rowSds'))

# extract ####
setGeneric('keyCol', function(x, ...) standardGeneric('keyCol'))
setGeneric('keyCol<-', function(x, ..., value) standardGeneric('keyCol<-'))


