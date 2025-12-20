## Test environments
- local: Windows 11, R 4.5.0
- win-builder: R-devel, R-release, R-oldrelease
- R-hub: linux (ubuntu-latest), macOS (macos-13)

## R CMD check results
0 errors | 0 warnings | 0 notes

## Notes
- Fixed useage of testthat package following package update.
- Package-level documentation updated to use `_PACKAGE` (roxygen2).
- Added `Encoding: UTF-8` and `Authors@R` in DESCRIPTION.
- Declared imports for `stats` RNG functions.
