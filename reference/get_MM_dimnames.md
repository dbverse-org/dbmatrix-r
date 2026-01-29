# get_MM_dimnames

Internal function to read row and column names of a .mtx file

## Usage

``` r
get_MM_dimnames(
  mtx_file_path,
  mtx_rowname_file_path,
  mtx_rowname_col_idx = 1,
  mtx_colname_file_path,
  mtx_colname_col_idx = 1,
  ...
)
```

## Arguments

- mtx_file_path:

  path to .mtx file to be read into database

- mtx_rowname_file_path:

  path to .mtx rowname file to be read into database. by default, no
  header is assumed.

- mtx_rowname_col_idx:

  column index of row name file

- mtx_colname_file_path:

  path to .mtx colname file to be read into database. by default, no
  header is assumed.

- mtx_colname_col_idx:

  column index of column name file

- ...:

  additional params to pass to
  [`data.table::fread()`](https://rdrr.io/pkg/data.table/man/fread.html)

## Value

list of row and column name character vectors

## Details

Can be used to read row and column names from .mtx files. Note: these
files must not contain a header (colnames).

The mtx_rowname_col_idx and mtx_colname_col_idx can be used to specify
the column index of the row and column name files, respectively. By
default, the first column is used for both.

TODO: Support for reading in only rownames or colnames.
