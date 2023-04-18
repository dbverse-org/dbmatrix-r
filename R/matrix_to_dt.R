

# create ijx vector representation of sparse matrix, keeping zeros
# Updates dgcmatrix by reference
# Copied from below:
# https://stackoverflow.com/questions/64473488/melting-a-sparse-matrix-dgcmatrix-and-keeping-its-zeros
#' @noRd
get_ijx_zero_dt <- function(x){
  dplyr::tibble(
    i=rownames(x)[row(x)],
    j=colnames(x)[col(x)],
    x=as.numeric(x)
  )
}


# Convert dgCMatrix into ijx vector DT
#' @noRd
get_ijx_dt <- function(dgCMatrix){

  mat_feats = dgCMatrix@Dimnames[[1]]
  names(mat_feats) = 1:dgCMatrix@Dim[[1]]

  mat_samples = dgCMatrix@Dimnames[[2]]
  names(mat_samples) = 1:dgCMatrix@Dim[[2]]

  matDT = data.table::as.data.table(Matrix::summary(dgCMatrix)) # convert dgCMatrix to dense matrix
  matDT[, c('i','j') := list(mat_feats[i], mat_samples[j])]

  return(matDT)
}
