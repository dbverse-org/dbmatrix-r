

# dbData object interactions ####
setGeneric('cPool', function(x, ...) standardGeneric('cPool'))
setGeneric('cPool<-', function(x, value, ...) standardGeneric('cPool<-'))
setGeneric('remoteName', function(x, ...) standardGeneric('remoteName'))
setGeneric('reconnect', function(x, ...) standardGeneric('reconnect'))
setGeneric('disconnect', function(x, ...) standardGeneric('disconnect'))
setGeneric('queryStack', function(x, ...) standardGeneric('queryStack'))
setGeneric('queryStack<-', function(x, value, ...) standardGeneric('queryStack<-'))
setGeneric('query', function(x, statement, ...) standardGeneric('query'))

# backend system interactions ####
setGeneric('validBE', function(x, ...) standardGeneric('validBE'))
setGeneric('listTablesBE', function(x, ...) standardGeneric('listTablesBE'))
setGeneric('existsTableBE', function(x, remote_name, ...) standardGeneric('existsTableBE'))
setGeneric('hashBE', function(x, ...) standardGeneric('hashBE'))
setGeneric('reconnectBackend', function(x, ...) standardGeneric('reconnectBackend'))
# setGeneric('setRemoteKey', function(x, remote_name, primary_key, ...) standardGeneric('setRemoteKey'))
