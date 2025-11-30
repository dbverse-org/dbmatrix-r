# get_MM_dim

Internal function to read dimensions of a .mtx file

## Usage

``` r
get_MM_dim(mtx_file_path)
```

## Arguments

- mtx_file_path:

  path to .mtx file to be read into database

## Value

integer vector of dimensions

## Details

Scans for the header of an mtx file (starting with %) and takes one more
line representing the dimensions and number of nonzero values.

Note: the header size can vary depending on the .mtx file.
