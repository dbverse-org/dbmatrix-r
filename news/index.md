# Changelog

## dbMatrix 0.0.0.9126 (2026-01-13)

### Bug fixes

- Update pkgdown workflow to include additional R packages and refs.

- Prevent NAs in show method by handling max.print truncation.

- Handle negative and out-of-order indexing.

- Require explicit opt-in for sparse-to-dense conversion.

- Cast comparison operators to numeric for type safety.

- Use direct slot access in transpose method.

- Conditionally check overwrite for dbMatrix creation.

- Improve verbosity messages and remove redundant logging in dbMatrix
  functions.

### Features

- Support transposed precompute tables.

- Add streaming SVD for dbSparseMatrix including fallback to
  BPCells::svds() for LTM data.

- Add Math SQL translations for
  [`log1p()`](https://rdrr.io/r/base/Log.html), `ln()` and
  [`log()`](https://rdrr.io/r/base/Log.html) functions.

- Add [`is.na()`](https://rdrr.io/r/base/NA.html) method.

### Performance

- Optimize dimname indexing with factors and
  [`match()`](https://rdrr.io/r/base/match.html) (dimname-returning
  functions return characters as expected).

### Refactoring

- Use temp tables and inline SQL (small queries still use inline SQL;
  otherwise register a temp table to avoid large string parsing
  bottlenecks).

- Centralize internal type-check functions in `dbProject`.

- Simplify summary methods to always return vectors.

- Rename dbVector internals and add column-wise recycling.

- Improve [`as.matrix()`](https://rdrr.io/r/base/matrix.html) and
  `compute()` for dbMatrix.

### Documentation

- Untrack docs/ folder.

- Consolidate class documentation and update vignettes.

### Chore

- Update build config and move package data to inst/extdata.

- Update package metadata and dependencies.

- Update roxygen.

- Update .gitignore to exclude compiled artifacts.

### Code style

- Reformat with air.

### Testing

- Tidy up tests and update to reflect recent code changes.

## dbMatrix 0.0.0.9125 (2025-12-04)

### Bug fixes

- Ensure as.matrix returns unnamed matrix when names=FALSE.

- Update show methods to handle duplicate dimnames via index filtering.

- Pkgdown errors.

- Ensure rowVars/colVars preserve empty dimensions.

- Make `compute` s3 and use CTAS instead.

### Features

- Optimize precompute with parquet and auto-attach existing tables.

- Implement native ingestion for various Matrix classes via
  `as.dbMatrix`.

- Add coercion methods for `dbMatrix` to matrix/Matrix objects.

- Add `.check_mem_limit` internal function.

- Support `overwrite` arg in `compute.dbMatrix`.

- `writeMM` method for dbMatrix objects.

- Add `value_colName` support to dbMatrix_from_tbl for pre-aggregated
  counts.

### Chore

- Update roxygen version.

- Ignore benchmarks/ repo.

- Update NAMESPACE with s3 exports.

### Documentation

- Tidy roxygen.

- Tidy + update global options in dbMatrix.

- Clean up roxygen comments & improve documentation formatting.

### Code style

- Tidy extract.R with `air` and `jarl`.

- Tidy .R files with `air` and `jarl`.

### Refactoring

- Delegate dbMatrix construction to as.dbMatrix generic for native R
  matrix ingestion.

- Clean up Math Summary ops so to use subqueries and R zero inflation.

### Performance

- Optimize sparse –\> matrix conversion.

- Improve mem safety of `as.matrix` for dense matrix conversions.

- Use head/tail filter pushdown in show method for dbMatrix objects.

- Improve `extract` by using `duckdb_register` for large indices.

- Optimize arith helpers with lazy queries instead of generating views.

### Testing

- Add tests for `compute`.

## dbMatrix 0.0.0.9124 (2025-10-14)

### Breaking Changes

- **Renamed [`load()`](https://rdrr.io/r/base/load.html) to
  `dbLoad()`**: The `load` method has been renamed to `dbLoad` to avoid
  masking [`base::load()`](https://rdrr.io/r/base/load.html). Generic
  moved to `dbProject` package for consistency across dbverse.

### Refactoring

- **S4 to S3 Method Conversion**: Converted
  [`rownames()`](https://rdrr.io/r/base/colnames.html),
  [`colnames()`](https://rdrr.io/r/base/colnames.html),
  [`nrow()`](https://rdrr.io/r/base/nrow.html),
  [`ncol()`](https://rdrr.io/r/base/nrow.html), and their setters from
  S4 to S3 methods for simpler dispatch. Only `%in%` remains S4
  (requires double dispatch). This significantly reduces package load
  messages.

- **Internal Function Cleanup**:

  - Converted `castNumeric` from S4 generic to internal function
    `.castNumeric()`
  - Renamed temporary tables to use `_tmp` prefix in row/column summary
    methods for consistency
  - Moved `dbList` generic to `dbProject` package

### Bug Fixes

- **Arrow Filter Pushdown Error**: Fixed “Arrow table filter pushdown
  optional: new_i IN (…) not supported yet” error by replacing
  [`arrow::to_duckdb()`](https://arrow.apache.org/docs/r/reference/to_duckdb.html)
  with inline SQL for extract operations.
- Fixed `.mtx` file reader header detection logic.

### Chore

- Applied consistent code formatting across the package.

## dbMatrix 0.0.0.9123 (2025-10-07)

### Features

- **dbProject Integration**: Full integration with `dbProject` package
  and `dbData` base class architecture. dbMatrix now inherits from
  `dbData` providing unified database-backed object interface across
  dbverse ecosystem.

- **Statistical Methods**: Added `rowSds()`, `colSds()`, `rowVars()`,
  and `colVars()` methods for both dbDenseMatrix and dbSparseMatrix.

- **dbVector Support**: Internal support for dbVector arithmetic
  operations.

### Chore

- Migrated repository to `dbverse-org` organization from previous
  location.
- Updated all URLs and links to reflect new `dbverse-org` organization
  structure.
- Required DuckDB \>= 1.4.0 (LTS) for improved stability.
- Updated documentation with markdown formatting improvements.
- Added `cli` to imports for better user messaging.

### Docs

- Added S4 class documentation to pkgdown reference index
  (dbMatrix-class, dbDenseMatrix-class, dbSparseMatrix-class).
- Updated vignettes to include `memory` and `names` parameters for
  summary functions.
- Improved roxygen documentation for `compute()`,
  [`as.matrix()`](https://rdrr.io/r/base/matrix.html), and arithmetic
  operations.
- Enhanced documentation for dbProject integration and dbData
  inheritance.

### Bug Fixes

- **Eliminated ORDER BY Warnings**: Fixed ORDER BY warnings by removing
  `arrange()` from lazy evaluation paths in summary methods. Now only
  applies ordering when materializing results to memory.
- Fixed
  [`sim_dgc()`](https://dbverse-org.github.io/dbmatrix-r/reference/simulate_objects.md)
  function to use correct number of random values (`rnorm(n_vals)`
  instead of `rnorm(num_cols)`), eliminating “number of items to
  replace” warnings.
- Removed deprecated `context()` call in test files (testthat 3rd
  edition).
- Fixed `to_view()` calls to pass `tbl` objects instead of dbMatrix
  objects to dbProject functions.
- Imported dbProject generics properly to avoid namespace conflicts.
- Added `init` slot to dbMatrix class definition.
- Removed redundant export for [`t()`](https://rdrr.io/r/base/t.html)
  method.
- Fixed [`names()`](https://rdrr.io/r/base/names.html) method to return
  NULL for regular matrices instead of throwing errors.
- Corrected name attribute assignment in
  [`Ops()`](https://rdrr.io/r/methods/S4groupGeneric.html) method for
  dbMatrix.
- Improved connection handling in internal
  [`get_con()`](https://dbverse-org.github.io/dbmatrix-r/reference/get_con.md)
  function.
- Removed `dbReconnect()` method from generics (moved to
  dbProject/dbData).
- Refactored extract methods - functionality now comes from
  `dbProject::dbData` base class.
- Exported dbMatrix class properly in NAMESPACE.
- Fixed DESCRIPTION dependencies: removed duplicate `dbProject` from
  Suggests, added proper Remotes reference.

## dbMatrix 0.0.0.9023 (2024-09-18)

### Breaking Changes

- dbMatrix summary methods now return `dbDenseMatrix` objects instead of
  in-memory vectors.

- Removed colTypes from castNumeric.

- Removed [@name](https://github.com/name) check in initialize.

### Features

- New `compute` method for saving dbMatrix objects. This writes the
  dbMatrix object to a table in the database along with its row and
  column names (e.g. ‘dbMatrixname_rownames’).

- New `load` method for loading computed dbMatrix objects. The
  dbMatrix::compute() method must be called before loading the object.

- Updated `.check_overwrite` to allow for passing overwrite arg.

- New internal function `write_dimnames` to enable saving dbMatrix
  objects.

- New `sum` method for dbMatrix objects.

- Improvements to precomputed table: Use existing precomputed table in
  db if available.

- Improvements to precomputed table: Transpose precomputed table of
  sufficient dimensions if it exists instead of creating a new one. This
  is done via a TEMPORARY VIEW to avoid writing to disk and modifying
  existing precomputed table which may be referenced by other tables.

- Custom SQL statement for `todbDense` conversion creates a TEMPORARY
  VIEW with name `dbDenseMatrix_hash`.

### Chore

- Update whitespace.

- Update dbMatrix constructor roxygen.

- Update roxygen in toDbDense.

### Docs

- Update reference page.

- Update mean documentation.

- Update log documentation.

- Update roxygen.

### Bug fixes

- Do not recycle matrix if not needed.

## dbMatrix 0.0.0.9022 (2024-08-14)

### Breaking changes

- Rename dbMatrix constructor to
  [`dbMatrix::dbMatrix()`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.md).

### Features

- Add more tests for `dbMatrix` and `dbDenseMatrix`.

- Add internal function `map_ijx_dimnames`.

- Add internal function `dbMatrix_from_tbl`.

- Extract now constructs unique temporary virtual tables in the arrow
  schema.

### Chore

- Document pkgdown website.

- Add `cli` to imports.

- Update generics.

### Bug fixes

- Be more explicit about non-supported `Arith` and `Ops` operations.

- Fix [`show()`](https://rdrr.io/r/methods/show.html) method for
  `dbDenseMatrix` objects.

## dbMatrix 0.0.0.9021 (2024-07-05)

### Bug fixes

- Improve show function for dbDenseMatrix.

- Updated simulate functions to include overwrite param.

- Update .check_overwrite() internal function to avoid bug in
  overwritting passed table.

## dbMatrix 0.0.0.9020 (2024-07-05)

### Features

- Add initial support for reading in .mtx files and creating dbMatrix
  objects.

### Bug fixes

- Update `check_overwrite` input validation

### Chore

- Update Royxgen

## dbMatrix 0.0.0.9019 (2024-03-18)

### Features

- Add new `precompute` function to speed up matrix densification.

- Add new show function for dbDenseMatrix with pretty color and better
  spacing.

- Add new `save` function to save a `dbMatrix`.

- Add new input validation functions.

### Chore

- Update docs.

- Update imports to include `glue`, `bit64` and `crayon`.

## dbMatrix 0.0.0.9018 (2024-02-12)

### Bug fixes

- Update constructor calls in `sim` functions.

- Remove redundant `con` from constructor.

- Remove `db_path` from constructor.

- Constructor `db_path` arg change to `con` object.

### Features

- Add `dgTMatrix` to in-memory matrix types supported in `dbMatrix`
  constructor.

### Chore

- Set :memory: to default db_path in constructor.

- Update createDBMatrix docs.

### Documentation

- Spacing.

- Update after constructor fix.

- Remove :temp: in place of :memory:.

## dbMatrix 0.0.0.9017 (2024-02-07)

### Bug fixes

- Remove “:temp:” from tests.

- Replace ‘:temp:’ with ‘:memory:’.

- Add matrix in addition to dgCMatrix in as_ijx().

### Features

- Add unit tests for scalar arith.

- Add unit tests for names.R.

### Chore

- Remove random browser() call.

- Add `MatrixGenerics` to deps.

- Update gitignore.

## dbMatrix 0.0.0.9016 (2024-01-23)

### Features

- Add boolean indexing tests to `test-extract.R`.

- Add as_ijx() convenience function.

- Update createDBMatrix() to use dplyr::copy_to().

- Update as_matrix() convenience function.

- Add unit tests for extract methods.

## dbMatrix 0.0.0.9015 (2024-01-22)

### Features

- Add log().

- Update toDbDense to use dplyr instead of SQL.

### Chore

- Update DESCRIPTION to include testthat.

- Update docs.

## dbMatrix 0.0.0.9014 (2024-01-19)

### Features

- update dbIndex superclass

- update as_matrix()

## dbMatrix 0.0.0.9013 (2024-01-18)

### Bug fixes

- Rename and update dbIndex superclass to fix indexing bugs.

### Features

- Add experimental as_matrix() convenience function.

### Chore

- Update docs.

- Migrate site link to drieslab.

- Move {Matrix} to Imports.

### Documentation

- Update overview.Rmd.

- Update operations vignette.

## dbMatrix 0.0.0.9012 (2023-12-08)

### Bug fixes

- Only densify if necessary. != 0, +/-.

- toDbDense() previously updated table by value. change to update by
  reference via VIEW creation of table named ‘dense’.

### Features

- Add dbListTables().

### Chore

- Update docs.

- Update gitignore.

## dbMatrix 0.0.0.9011 (2023-12-08)

### Bug fixes

- Fix incorrect aggregate operations.

- Fixes after con slot removal.

### Features

- Add get_con().

- Add dbDisconnect() generic.

- Update accessors.

### Chore

- Update vignettes.

- Update README.

- Update gitignore.

- Update docs.

- Update NEWS.

## dbMatrix 0.0.0.9010 (2023-11-25)

### Breaking changes

- migrate to \|\> pipe, remove %\>%, update deps.

### Chore

- Update docs.

## dbMatrix 0.0.0.9009 (2023-11-20)

### Bug fixes

- Mean generic for dbDenseMatrix.

### Chore

- Update docs.

- Update site.

- Update gitignore.

## dbMatrix 0.0.0.9008 (2023-11-17)

### Bug fixes

- Updates to dbSparseMatrix rowMeans, colMeans.

- Updates to colSums and rowSums for dbSparseMatrix.

- Update dbMatrix constructor dimnames issues.

### Chore

- Update DESCRIPTION, remove redundant Matrix import, dep.

- Update docs.

- Update roxygen for operations.

## dbMatrix 0.0.0.9007 (2023-11-09)

### Features

- Add Matrix as dep.

## dbMatrix 0.0.0.9006 (2023-11-09)

### Bug fixes

- Methods::as scope correction.

### Features

- Add methods to deps.

### Chore

- Update .gitignore.

## dbMatrix 0.0.0.9005 (2023-11-09)

### Feat

- Add Matrix as pkg dep

## dbMatrix 0.0.0.9004 (2023-11-09)

### Bug fixes

- Missing Matrix:: scope call.

### Chore

- Update docs.

## dbMatrix 0.0.0.9003 (2023-11-07)

### Fix

- `dims` and `dim_names` in `createDBMatrix()` retained from in-memory
  matrix or Matrix object

### Chore

- Update docs for dbMatrix input checks

## dbMatrix 0.0.0.9002 (2023-11-07)

### Feat

- Specify checks for ‘name’ param in `createDBMatrix()`

### Chore

- Update docs.

- Add docs.

## dbMatrix 0.0.0.9001 (2023-11-04)

### Chore

- Update docs and DESCRIPTION.

- Migration dbMatrix.
