# Coerce dbMatrix to matrix

Coercion methods to convert `dbMatrix` objects to in-memory `matrix`
objects. Respects `dbMatrix.max_mem_convert` option to prevent OOM
errors.

## Value

A base R [`matrix`](https://rdrr.io/r/base/matrix.html) containing the
collected matrix values with the same dimensions and dimnames as the
source object.
