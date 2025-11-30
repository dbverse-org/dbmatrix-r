# Generate array for pretty printing of matrix values

Generate array for pretty printing of matrix values

## Usage

``` r
print_array(
  i = NULL,
  j = NULL,
  x = NULL,
  dims,
  rownames = rep("", dims[1]),
  class = c("sparse", "dense"),
  fill = ".",
  digits = 5L
)
```

## Arguments

- i, j, x:

  matched vectors of integers in i and j, with value in x

- dims:

  dimensions of the array (integer vector of 2)

- fill:

  fill character

- digits:

  default = 5. If numeric, round to this number of digits
