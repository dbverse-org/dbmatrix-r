# Package-wide global variables to avoid R CMD check NOTEs
# These are used in NSE contexts (dplyr, dbplyr)

utils::globalVariables(c(
  # Column names used in dbplyr/dplyr
  "i", "j", "x", "n",
  "i_orig", "j_orig",
  "new_i", "new_j", "new_value",
  "idx", ".I",
  
  # Summary statistics column names
  "sum_x", "sum_x2", "mean_x", "var_x", "sd_x",
  "scale_val",
  
  # Matrix names file columns
  "..mtx_colname_col_idx", "..mtx_rowname_col_idx",
  
  # Coercion variables
  "x.i", "x.x", "x.y",
  
  # Temporary lookup table columns
  ".col_name_tmp", ".row_name_tmp"
))
