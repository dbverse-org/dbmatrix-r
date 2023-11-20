# dbData object interactions ####
# setGeneric('cPool', function(x, ...) standardGeneric('cPool'))
# setGeneric('cPool<-', function(x, ..., value) standardGeneric('cPool<-'))
# setGeneric('remoteName', function(x, ...) standardGeneric('remoteName'))
# setGeneric('reconnect', function(x, ...) standardGeneric('reconnect'))
# setGeneric('disconnect', function(x, ...) standardGeneric('disconnect'))
# setGeneric('queryStack', function(x, ...) standardGeneric('queryStack'))
# setGeneric('queryStack<-', function(x, ..., value) standardGeneric('queryStack<-'))
# setGeneric('sql_query', function(x, statement, ...) standardGeneric('sql_query'))
setGeneric('colTypes', function(x, ...) standardGeneric('colTypes'))
setGeneric('castNumeric', function(x, col, ...) standardGeneric('castNumeric'))
# setGeneric('create_index', function(x, name, column, unique, ...) standardGeneric('create_index'))
# setGeneric('is_init', function(x, ...) standardGeneric('is_init'))

# dbMatrix specific ####
setGeneric('colSds', function(x, ...) standardGeneric('colSds'))
setGeneric('colMeans', function(x, ...) standardGeneric('colMeans'))
setGeneric('colSums', function(x, ...) standardGeneric('colSums'))
setGeneric('rowSds', function(x, ...) standardGeneric('rowSds'))
setGeneric('rowMeans', function(x, ...) standardGeneric('rowMeans'))
setGeneric('rowSums', function(x, ...) standardGeneric('rowSums'))

# dbData ops ####
setGeneric('t', function(x, ...) standardGeneric('t'))
setGeneric('mean', function(x, ...) standardGeneric('mean'))

# extract ####
# setGeneric('keyCol', function(x, ...) standardGeneric('keyCol'))
# setGeneric('keyCol<-', function(x, ..., value) standardGeneric('keyCol<-'))


