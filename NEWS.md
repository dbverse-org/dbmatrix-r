<!-- NEWS.md is maintained by https://fledge.cynkra.com, contributors should not edit this file -->

# dbMatrix 0.0.0.9012 (2023-12-08)

## Bug fixes

- Only densify if necessary. != 0, +/-.

- toDbDense() previously updated table by value. change to update by reference via VIEW creation of table named 'dense'.

## Features

- Add dbListTables().

## Chore

- Update docs.

- Update gitignore.


# dbMatrix 0.0.0.9011 (2023-12-08)

## Bug fixes

- Fix incorrect aggregate operations.

- Fixes after con slot removal.

## Features

- Add get_con().

- Add dbDisconnect() generic.

- Update accessors.

## Chore

- Update vignettes.

- Update README.

- Update gitignore.

- Update docs.

- Update NEWS.


# dbMatrix 0.0.0.9010 (2023-11-25)

## Breaking changes

- migrate to |> pipe, remove %>%, update deps.

## Chore

- Update docs.


<!-- NEWS.md is maintained by https://fledge.cynkra.com, contributors should not edit this file -->

# dbMatrix 0.0.0.9009 (2023-11-20)

## Bug fixes

- Mean generic for dbDenseMatrix.

## Chore

- Update docs.

- Update site.

- Update gitignore.


# dbMatrix 0.0.0.9008 (2023-11-17)

## Bug fixes

- Updates to dbSparseMatrix rowMeans, colMeans.

- Updates to colSums and rowSums for dbSparseMatrix.

- Update dbMatrix constructor dimnames issues.

## Chore

- Update DESCRIPTION, remove redundant Matrix import, dep.

- Update docs.

- Update roxygen for operations.


# dbMatrix 0.0.0.9007 (2023-11-09)

## Features

- Add Matrix as dep.


# dbMatrix 0.0.0.9006 (2023-11-09)

## Bug fixes

- Methods::as scope correction.

## Features

- Add methods to deps.

## Chore

- Update .gitignore.

# dbMatrix 0.0.0.9005 (2023-11-09)

## Feat

- Add Matrix as pkg dep


# dbMatrix 0.0.0.9004 (2023-11-09)

## Bug fixes

- Missing Matrix:: scope call.

## Chore

- Update docs.

# dbMatrix 0.0.0.9003 (2023-11-07)

## Fix

- `dims` and `dim_names` in `createDBMatrix()` retained from in-memory matrix or Matrix object

## Chore

- Update docs for dbMatrix input checks

# dbMatrix 0.0.0.9002 (2023-11-07)

## Feat
- Specify checks for 'name' param in `createDBMatrix()`

## Chore

- Update docs.

- Add docs.


# dbMatrix 0.0.0.9001 (2023-11-04)

## Chore

- Update docs and DESCRIPTION.

- Migration dbMatrix.
