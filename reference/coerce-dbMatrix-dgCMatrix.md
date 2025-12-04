# Coerce dbMatrix to dgCMatrix

Coercion methods to convert `dbMatrix` objects to in-memory `dgCMatrix`
objects. Respects `dbMatrix.max_mem_convert` option to prevent OOM
errors.
