# dbData object interactions ####
setGeneric('colTypes', function(x, ...) standardGeneric('colTypes'))
setGeneric('castNumeric', function(x, col, ...) standardGeneric('castNumeric'))

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


