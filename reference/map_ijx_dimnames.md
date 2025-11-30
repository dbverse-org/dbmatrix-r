# Map dimnames to i,j indices

Map dimnames to i,j indices

## Usage

``` r
map_ijx_dimnames(dbMatrix, colName_i, colName_j)
```

## Arguments

- dbMatrix:

  dbMatrix object

- colName_i:

  name of column rownames to add to database

- colName_j:

  name of column colnames to add to database default: 'FALSE'.'

## Details

Constructs a table in a database that contains the accompanying dimnames
for a dbMatrix. The resulting columns in the table:

- i (row index)

- colName_i (rownames),

- j (col index)

- j_names (colnames)

- x (counts of i,j occcurences)
