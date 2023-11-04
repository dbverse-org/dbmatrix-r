## ---- message=F---------------------------------------------------------------
# git clone repo
devtools::load_all('../')
library(dplyr)
library(duckdb)

## ----eval=TRUE, message=FALSE, warning=FALSE----------------------------------
# simulate dbSparseMatrix
sparse = sim_dbSparseMatrix()

# preview 
dplyr::glimpse(sparse[]) 

