



#' @title Read matrix
#' @name readMatrixR
#' @description Function to read a matrix in from flat file via R.
#' @param path path to the matrix file
#' @param cores number of cores to use
#' @param transpose transpose matrix
#' @return matrix
#' @details The matrix needs to have both unique column names and row names
#' @export
readMatrixR = function(path,
                       cores = 1L,
                       transpose = FALSE) {

  # check if path is a character vector and exists
  if(!is.character(path)) stop('path needs to be character vector')
  path = path.expand(path)
  if(!file.exists(path)) stop('the file: ', path, ' does not exist')


  data.table::setDTthreads(threads = cores)

  # read and convert
  DT = suppressWarnings(data.table::fread(input = path, nThread = cores))
  mtx = as.matrix(DT[,-1])



  if(isTRUE(transpose)) {
    mtx = Matrix::Matrix(data = mtx,
                         dimnames = list(DT[[1]], colnames(DT[,-1])),
                         sparse = TRUE)
    mtx = Matrix::t(mtx)
  }

  return(mtx)
}





# TODO read .mtx format (uses dictionaries and has no 0 values. Already in ijx)

# TODO read .parquet (arrow::read_parquet(), arrow::to_duckdb(), tidyr::pivot_longer())




