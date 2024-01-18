<!-- NEWS.md is maintained by https://fledge.cynkra.com, contributors should not edit this file -->

# dbMatrix 0.0.0.9013 (2024-01-18)

## Bug fixes

- Densify conditional.

- Add dev to gha workflow.

- Only densify if necessary. != 0, +/-.

- ToDbDense() previously updated table by value. change to update by reference via VIEW creation of table named 'dense'.

- Bug in calculation of aggregate operations.

- Housekeeping after con slot removal.

- Mean for dbSparseMatrix.

- Mean generic for dbDenseMatrix.

- Updates to dbSparseMatrix rowMeans, colMeans.

- Updates to colSums and rowSums for dbSparseMatrix.

- Update dbMatrix constructor dimnames issues.

- Methods::as scope correction.

## Features

- Add as_matrix() convenience function.

- Add as_matrix() function.

- Add dbListTables().

- Add dbListTables.

- Add dbDisconnect to NAMESPACE.

- Add dbDisconnect generic.

- Update accessors and add get_con().

- Add Matrix as dep.

- Add methods to deps.

## Chore

- Update docs.

- Remove browser() call.

- Remove browser().

- Migrate site links to drieslab.

- Migrate site ilnks to drieslab.

- Update vignettes.

- Update docs.

- Update gitignore.

- Update vignettes.

- Update docs.

- Update gitignore.

- Update docs.

- Update docs.

- Remove `googledrive` from imports.

- Update docs.

- Add `googledrive` to imports.

- Update docs.

- Update docs.

- Update NEWS.

- Update docs.

- Update docs.

- Update docs.

- Update README.

- Update docs.

- Update docs.

- Update docs.

- Update site.

- Update gitignore.

- Update DESCRIPTION, remove redundant Matrix import, dep.

- Update docs.

- Update roxygen for operations.

- Update .gitignore.

- Updates.

- Update docs.

- Delete .Rhistory.

- Update docs.

- Update docs, gitignore, tests/.

- Update docs.

- Update ignore.

- Add docs.

- dbMatrix migration, update docs and DESCRIPTION.

- Migration dbMatrix.

## Documentation

- Update overview.Rmd.

- Update operations vignette.

## Uncategorized

- Remove fpeek.

- Fix bug in getting backendInfo.

- Fix bug in reading number of rows.

- Merge branch 'main' of https://github.com/jiajic/GiottoDB.

- Merge branch 'drieslab:main' into main.

- Add dbPointsProxy creation.

- Add improved table creation pipeline to allow PK and col data type setting separately from defaults detection.

- Add extract generics for dbPointsProxy.

- Added querying of dbPolygonProxy and dbPointsProxy.

- Updated file reading and exposed method of adding custom fields information during DB table creation.

- Fleshed out implementations of database spatial point and polygon represenations.

- PLACEHOLDER https://github.com/drieslab/dbMatrix/pull/17 (#17).

- PLACEHOLDER https://github.com/drieslab/dbMatrix/pull/15 (#15).

- PLACEHOLDER https://github.com/drieslab/dbMatrix/pull/14 (#14).

- PLACEHOLDER https://github.com/drieslab/dbMatrix/pull/13 (#13).

- PLACEHOLDER https://github.com/drieslab/dbMatrix/pull/12 (#12).

- PLACEHOLDER https://github.com/drieslab/dbMatrix/pull/11 (#11).

- PLACEHOLDER https://github.com/drieslab/dbMatrix/pull/10 (#10).

- PLACEHOLDER https://github.com/drieslab/dbMatrix/pull/9 (#9).

- PLACEHOLDER https://github.com/drieslab/dbMatrix/pull/8 (#8).

- PLACEHOLDER https://github.com/drieslab/dbMatrix/pull/7 (#7).

- PLACEHOLDER https://github.com/drieslab/dbMatrix/pull/6 (#6).

- PLACEHOLDER https://github.com/drieslab/dbMatrix/pull/5 (#5).

- PLACEHOLDER https://github.com/drieslab/dbMatrix/pull/4 (#4).

- PLACEHOLDER https://github.com/drieslab/dbMatrix/pull/3 (#3).

- PLACEHOLDER https://github.com/drieslab/dbMatrix/pull/2 (#2).

- PLACEHOLDER https://github.com/drieslab/dbMatrix/pull/1 (#1).


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
