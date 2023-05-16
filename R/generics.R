

# dbData object interactions ####
setGeneric('cPool', function(x, ...) standardGeneric('cPool'))
setGeneric('cPool<-', function(x, ..., value) standardGeneric('cPool<-'))
setGeneric('remoteName', function(x, ...) standardGeneric('remoteName'))
setGeneric('reconnect', function(x, ...) standardGeneric('reconnect'))
setGeneric('disconnect', function(x, ...) standardGeneric('disconnect'))
setGeneric('queryStack', function(x, ...) standardGeneric('queryStack'))
setGeneric('queryStack<-', function(x, ..., value) standardGeneric('queryStack<-'))
setGeneric('query', function(x, statement, ...) standardGeneric('query'))
setGeneric('colTypes', function(x, ...) standardGeneric('colTypes'))
setGeneric('castNumeric', function(x, col, ...) standardGeneric('castNumeric'))

# backend system interactions ####
setGeneric('validBE', function(x, ...) standardGeneric('validBE'))
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


